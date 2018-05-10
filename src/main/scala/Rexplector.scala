import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import com.sun.jdi._
import com.sun.jdi.event.{Event, MethodEntryEvent, VMDeathEvent, VMStartEvent}
import com.sun.jdi.request.EventRequest
import io.circe.generic.auto._
import io.circe.syntax._

import scala.collection.mutable
import scala.io.Source

object Rexplector {

    val pattern = "a?a?a?a?a?a?a?a?a?a?aaaaaaaaaa"
    val input = "aaaaaaaaaa"
    val tracer = new Tracer()

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

    private[this] def dump(thread: ThreadReference, method: Method): Unit = {
        if (method.name() == "match") {
            val topframe = thread.frame(0)
            val args = topframe.getArgumentValues
            if (args.size() == 3) {
                val pos = args.get(1).asInstanceOf[IntegerValue].value()
                val seq = args.get(2).asInstanceOf[StringReference].value()
                tracer.step(thread.frame(1).thisObject().uniqueID(),
                            (topframe.thisObject().uniqueID(), shortName(method.declaringType().name())),
                            pos)
            }
        }
    }

    private[this] def shortName(name: String): String = {
        name.lastIndexOf('$') match {
            case -1 => name
            case dollarAt => name.substring(dollarAt + 1)
        }
    }

    private[this] def writeReport(): Unit = {
        case class Node(id: Long, label: String)
        case class Edge(from: Long, to: Long, id: String, arrows: String = "to", selectionWidth: Int = 5)
        case class Step(from: Long, to: Long, pos: Int)

        val nodes = tracer.names.map(Node.tupled).asJson
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
               |<p>$pattern </p>
               |<p id="inp"></p>
               |
               |<script type="text/javascript">
               |  var nodes = new vis.DataSet($nodes);
               |  var edges = new vis.DataSet($edges);
               |  var steps = $steps;
               |  var input = ${input.asJson};
               |
               |  var container = document.getElementById('mynetwork');
               |  var data = {
               |    nodes: nodes,
               |    edges: edges
               |  };
               |  var options = {
               |  };
               |  var network = new vis.Network(container, data, options);
               |
               |  var inpElement = document.getElementById('inp');
               |
               |  document.getElementById('stepper').addEventListener('change', function (event) {
               |    var step = steps[event.target.value - 1];
               |    network.setSelection({nodes: [step.from], edges: [step.from + "-" + step.to]},
               |                         {highlightEdges: false });
               |    inpElement.innerHTML = input.substring(0, step.pos) + "<b>" + input.substring(step.pos, step.pos + 1) + "</b>" + input.substring(step.pos + 1);
               |  });
               |</script>
               |</body>
               |</html>
               |
         """.stripMargin
        Files.write(Paths.get("/tmp/spam.html"), output.getBytes(StandardCharsets.UTF_8))
    }
}


class Tracer {

    val names: mutable.Map[Long, String] = mutable.Map()
    var edges: Seq[(Long, Long)] = Seq()
    var steps: Seq[(Int, (Long, Long))] = Seq()

    def step(from: Long, to: (Long, String), pos: Int): Unit = {
        names(to._1) = to._2
        edges = edges :+ (from, to._1)
        steps = steps :+ (pos, (from, to._1))
    }
}