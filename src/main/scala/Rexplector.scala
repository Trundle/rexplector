import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import com.sun.jdi._
import com.sun.jdi.event._
import com.sun.jdi.request.EventRequest
import io.circe.generic.auto._
import io.circe.syntax._

import scala.collection.mutable
import scala.io.Source

object Rexplector {

    val pattern = "(ab)+a"
    val input = "ababa"
    val tracer = new Tracer()
    var previousCursor: Int = 0
    var realPattern: String = _
    // Fields of Pattern (during compilation)
    var cursorField: Field = _
    var tempField: Field = _

    def main(args: Array[String]): Unit = {
        val launchingConnector = Bootstrap.virtualMachineManager().defaultConnector()
        val args = launchingConnector.defaultArguments()
        args.get("main").setValue(s"Runner ${quote(pattern)} $input")
        args.get("suspend").setValue("true")
        args.get("options").setValue("-cp /tmp")
        val vm = launchingConnector.launch(args)

        breakOnMethodEntry(vm)

        var exit = false
        while (!exit) {
            val eventSet = vm.eventQueue().remove()
            val iter = eventSet.iterator()
            while (iter.hasNext) {
                exit = handleEvent(iter.next())
            }
        }

        writeReport()
    }

    private[this] def quote(value: String): String = "\"" + value + "\""

    private[this] def breakOnMethodEntry(vm: VirtualMachine): Unit = {
        val methodEntryRequest = vm.eventRequestManager().createMethodEntryRequest()
        methodEntryRequest.addClassFilter("java.util.regex.*")
        methodEntryRequest.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD)
        methodEntryRequest.enable()
    }

    private[this] def handleEvent(event: Event): Boolean = {
        event match {
            case _: VMStartEvent =>
                event.virtualMachine().resume()
                false
            case _: VMDeathEvent =>
                val errorStream = event.virtualMachine().process().getErrorStream
                println(Source.createBufferedSource(errorStream).mkString)
                true
            case methodEntryEvent: MethodEntryEvent =>
                dump(methodEntryEvent.thread(), methodEntryEvent.method())
                methodEntryEvent.thread().resume()
                false
            case _ =>
                println(event)
                false
        }
    }

    // XXX bad name dump
    private[this] def dump(thread: ThreadReference, method: Method): Unit = {
        method.name() match {
            case "match" =>
                val topframe = thread.frame(0)
                val args = topframe.getArgumentValues
                if (args.size() == 3) {
                    val pos = args.get(1).asInstanceOf[IntegerValue].value()
                    tracer.step(thread.frame(1).thisObject().uniqueID(),
                                topframe.thisObject().uniqueID(),
                                pos)
                }
            case "<init>" =>
                // Node is the base class, hence if we look only at Node's constructor and not at the constructors
                // of the derived classes, we should see every Node instance exactly once, even from derived classes
                method.declaringType().name() match {
                    case "java.util.regex.Pattern$Node" =>
                        findPattern(thread) match {
                            case Some(pattern) =>
                                traceNode(thread, pattern)
                                if (realPattern == null) {
                                    realPattern = extractPattern(pattern, 0)
                                }
                            case None => traceStaticNode(thread, method)
                        }
                    case "java.util.regex.Matcher" =>
                        tracer.node(thread.frame(0).thisObject().uniqueID(), "Matcher", pattern)
                    case _ =>
                }
            case "compile" =>
                cursorField = method.declaringType().fieldByName("cursor")
                tempField = method.declaringType().fieldByName("temp")
            case _ =>
        }
    }

    private[this] def traceNode(thread: ThreadReference, pattern: ObjectReference): Unit = {
        val currentCursor = pattern.getValue(cursorField).asInstanceOf[IntegerValue].value()
        val nodeObj = thread.frame(0).thisObject()
        val nodePattern = extractPattern(pattern, previousCursor, Some(currentCursor))
        tracer.node(nodeObj.uniqueID(), shortName(nodeObj.referenceType().name()), nodePattern,
                    Some((previousCursor, currentCursor)))
        previousCursor = currentCursor
    }

    private[this] def traceStaticNode(thread: ThreadReference, method: Method): Unit = {
        val nodeObj = thread.frame(0).thisObject()
        tracer.node(nodeObj.uniqueID(), shortName(nodeObj.referenceType().name()),
                    method.location().toString)
    }

    private[this] def extractPattern(pattern: ObjectReference, from: Int, to: Option[Int] = None): String = {
        val temp = pattern.getValue(tempField).asInstanceOf[ArrayReference]
        val nodePatternPoints = new Array[Int](to.getOrElse(temp.length()) - from)
        for (i <- 0 until nodePatternPoints.length) {
            nodePatternPoints(i) = temp.getValue(from + i).asInstanceOf[IntegerValue].value()
        }
        new String(nodePatternPoints, 0, nodePatternPoints.length)
    }

    private[this] def findPattern(thread: ThreadReference): Option[ObjectReference] = {
        (0 until thread.frameCount())
            .map(thread.frame(_).thisObject())
            .filter(_ != null)
            .filter(_.referenceType().name() == "java.util.regex.Pattern")
            .headOption
    }

    private[this] def shortName(name: String): String = {
        name.lastIndexOf('$') match {
            case -1 => name
            case dollarAt => name.substring(dollarAt + 1)
        }
    }

    private[this] def writeReport(): Unit = {
        case class GraphNode(id: Long, label: String)
        case class Edge(from: Long, to: Long, id: String, arrows: String = "to", selectionWidth: Int = 5)
        case class Step(from: Long, to: Long, pos: Int)
        case class PatternPart(from: Int, to: Int)

        val connectedNodes = tracer.edges.map { case (from, to) => Set(from, to) }.flatten.toSet[Long]
        val usedNodes = tracer.nodes.filterKeys(connectedNodes.contains)
        val graphNodes = usedNodes.map { case (id, (label, _)) => GraphNode(id, label) }.asJson
        val parts = usedNodes
            .filter { case (_, (_, pos)) => pos.isDefined }
            .map { case (id, (_, Some(pos))) => (id, PatternPart(pos._1, pos._2)) }
            .toMap[Long, PatternPart]
            .asJson
        val edges = tracer.edges.map { case (from, to) => Edge(from, to, s"$from-$to") }.distinct.asJson
        val steps = tracer.steps.map { case (pos, (from, to)) => Step(from, to, pos) }.asJson
        val output =
            s"""
               |<!doctype html>
               |<html>
               |<head>
               |  <title>Network | Basic usage</title>
               |
               |  <script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/vis/4.21.0/vis.min.js"></script>
               |  <link href="https://cdnjs.cloudflare.com/ajax/libs/vis/4.21.0/vis.min.css" rel="stylesheet" type="text/css" />
               |
               |  <style type="text/css">
               |    #mynetwork {
               |      width: 1024px;
               |      height: 600px;
               |      border: 1px solid lightgray;
               |    }
               |  </style>
               |</head>
               |<body>
               |
               |<div id="mynetwork"></div>
               |<input type="range" min="1" max="${tracer.steps.size}" value="1" id="stepper" />
               |<p id="pattern"></p>
               |<p id="inp"></p>
               |
               |<script type="text/javascript">
               |  var nodes = new vis.DataSet($graphNodes);
               |  var edges = new vis.DataSet($edges);
               |  var steps = $steps;
               |  var input = ${input.asJson};
               |  var pattern = ${realPattern.asJson};
               |  var parts = $parts;
               |
               |  var container = document.getElementById('mynetwork');
               |  var data = {
               |    nodes: nodes,
               |    edges: edges
               |  };
               |  var options = {};
               |  var network = new vis.Network(container, data, options);
               |
               |  var inpElement = document.getElementById('inp');
               |  var patternElement = document.getElementById('pattern');
               |
               |  document.getElementById('stepper').addEventListener('change', function (event) {
               |    var stepNumber = event.target.value - 1;
               |    if (stepNumber >= 0) {
               |      setStep(stepNumber);
               |    }
               |  });
               |
               |  function setStep(stepNumber) {
               |    var step = steps[stepNumber];
               |    network.setSelection({nodes: [step.to], edges: [step.from + "-" + step.to]},
               |                         {highlightEdges: false});
               |    inpElement.innerHTML = highlight(input, "|", "", step.pos, step.pos + 1);
               |    var part = parts[step.to];
               |    if (part) {
               |      patternElement.innerHTML = highlight(pattern, "<b>", "</b>", part.from, part.to);
               |    } else {
               |      patternElement.innerHTML = pattern;
               |    }
               |  }
               |
               |  function highlight(value, opening, closing, from, to) {
               |      return (
               |        value.substring(0, from)
               |        + opening + value.substring(from, to) + closing
               |        + value.substring(to)
               |      );
               |  }
               |
               |  setStep(0);
               |</script>
               |</body>
               |</html>
               |
         """.stripMargin
        Files.write(Paths.get("/tmp/spam.html"), output.getBytes(StandardCharsets.UTF_8))
    }
}


class Tracer {

    val nodes: mutable.Map[Long, (String, Option[(Int, Int)])] = mutable.Map()
    var edges: Seq[(Long, Long)] = Seq()
    var steps: Seq[(Int, (Long, Long))] = Seq()

    def node(id: Long, name: String, pattern: String, part: Option[(Int, Int)] = None): Unit = {
        nodes(id) = (s"$name ($pattern)", part)
    }

    def step(from: Long, to: Long, pos: Int): Unit = {
        edges = edges :+ (from, to)
        steps = steps :+ (pos, (from, to))
    }
}