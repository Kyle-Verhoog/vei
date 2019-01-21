import jlalr.Error;
import jlalr.Grammar;
import jlalr.Production;

import java.util.Scanner;

// testing the // single
// line
public class Empty {
    // comments  public static void main(String[] args)

    public static void main(String[] args) {
        int i = 0;
        char a = 'a';
        String kevin = "kevin";
        boolean b = true;
        String test = null;
        System.out.println("Helloworld");
    }

    public static int toInt(String line, String msg) {
        return new Integer(line);
    }
    public static Grammar readGrammar(Scanner in) {
        Grammar grammar = new Grammar();
        String line = readLine(in, "Expecting number of non-terminals");
        int nterm = toInt(line, "number of non-terminals");
        for(int i = 0; i < nterm; i=i+1) {
            String term = readLine(in, "Expecting a non-terminal").intern();
        }

        line = readLine(in, "Expecting number of non-terminals");
        int nnonterm = toInt(line, "number of non-terminals");
        for(int i = 0; i < nnonterm; i=i+1) {
            String nonterm = readLine(in, "Expecting a non-terminal").intern();
        }

        line = readLine(in, "Expecting number of productions");
        int nprods = toInt(line, "number of productions");
        for(int i = 0; i < nprods; i=i+1) {
            Production prod = readProduction(readLine(in, "Expecting production"), grammar);
        }
        if(in.hasNextLine()) {
            System.err.println("Warning: extra input lines after grammar; maybe your production count is wrong.");
        }
        return grammar;
    }

    public static void test_if_statements() {
        int i = 1;
        int j = 2;

        if (i == j)
            return i;

        if (i == j)
            return i;
        else
            return j;

        if (i == j) {
        }
    }
}

