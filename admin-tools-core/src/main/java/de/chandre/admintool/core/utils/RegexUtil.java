package de.chandre.admintool.core.utils;

/**
 * util for regular expressions
 * 
 * @author Andre
 *
 */
public class RegexUtil {
	
	private RegexUtil()
    {

    }

    /**
     * transforms a wildcard to an regex
     * 
     * @see http://www.rgagnon.com/javadetails/java-0515.html
     * 
     * @param wildcard
     * @return
     */
    public static String wildcardToRegex(String wildcard)
    {
        StringBuffer s = new StringBuffer(wildcard.length());
        s.append('^');
        for (int i = 0, is = wildcard.length(); i < is; i++)
        {
            char c = wildcard.charAt(i);
            switch (c)
            {
                case '*':
                    s.append(".*");
                    break;
                case '?':
                    s.append(".");
                    break;
                // escape special regexp-characters
                case '(':
                case ')':
                case '[':
                case ']':
                case '$':
                case '^':
                case '.':
                case '{':
                case '}':
                case '|':
                case '\\':
                    s.append("\\");
                    s.append(c);
                    break;
                default:
                    s.append(c);
                    break;
            }
        }
        s.append('$');
        return (s.toString());
    }
}
