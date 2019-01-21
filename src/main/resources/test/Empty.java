import jlalr.Error;
import jlalr.Grammar;
import jlalr.Production;

import java.util.Scanner;

public class Empty {
    public static void main(String[] args) {
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
}