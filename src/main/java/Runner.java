import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Runner {

    public static void main(final String... args) {
        System.err.println(args[0]);
        System.err.println(args[1]);
        final Pattern pattern = Pattern.compile(args[0]);
        final Matcher matcher = pattern.matcher(args[1]);
        matcher.matches();
    }
}
