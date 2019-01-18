import jlalr.Error;
import jlalr.Grammar;
import jlalr.Production;

import java.util.Scanner;

public class Empty {
    public static void main(String[] args) {
        'a'
        System.out.println("Helloworld");
    }
    public static int toInt(String line, String msg) {
        try {
            return new Integer(line);
        } catch(NumberFormatException e) {
            throw new jlalr.Error("Expecting "+msg+" but the line is \"not a number:\n"+line);
        }
    }
    public static Grammar readGrammar(Scanner in) {
        Grammar grammar = new Grammar();
        String line = readLine(in, "Expecting number of non-terminals");
        int nterm = toInt(line, "number of non-terminals");
        for(int i = 0; i < nterm; i++) {
            String term = readLine(in, "Expecting a non-terminal").intern();
            if(!grammar.terminals.add(term))
                throw new jlalr.Error("Duplicate terminal: "+term);
        }

        line = readLine(in, "Expecting number of non-terminals");
        int nnonterm = toInt(line, "number of non-terminals");
        for(int i = 0; i < nnonterm; i++) {
            String nonterm = readLine(in, "Expecting a non-terminal").intern();
            if(!grammar.nonterminals.add(nonterm))
                throw new jlalr.Error("Duplicate non-terminal: "+nonterm);
            if(grammar.isTerminal(nonterm))
                throw new jlalr.Error("Cannot be both terminal and non-terminal: "+nonterm);
        }

        grammar.start = readLine(in, "Expecting start symbol").intern();
        if(!grammar.nonterminals.contains(grammar.start)) throw new jlalr.Error(
                "Start symbol "+grammar.start+" was not declared as a non-terminal.");

        line = readLine(in, "Expecting number of productions");
        int nprods = toInt(line, "number of productions");
        for(int i = 0; i < nprods; i++) {
            Production prod = readProduction(readLine(in, "Expecting production"), grammar);
            if(!grammar.productions.add(prod)) {
                throw new jlalr.Error("Duplicate production: "+prod);
            }
        }
        if(in.hasNextLine()) {
            System.err.println("Warning: extra input lines after grammar; maybe your production count is wrong.");
        }
        return grammar;
    }
}