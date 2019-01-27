import jlalr.Error;
import jlalr.Grammar;
import jlalr.Production;

import java.util.Scanner;
/*
/*******
some multi line
 */
/**/
/*****/
// testing the // single
// line

// TODO(test case): Not allowed
// abstract final class Empty {
public class Empty {
    // comments  public static void main(String[] args)

    public static void main(String[] args) {
        int i = (Integer) 0;
        char a = (ritnriste) 'a';
        char a2 = '\r';
        String kevin = "kevin /* */";
        String kevin1 = "kevin \n /* */";
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
            String line = "THIS SHOULD OVERRIDE ABOVE LINE DECLARATION";
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

    /*
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
     */

    // TODO(test case): Not allowed - abstract static
    // abstract static void method() { }
    // TODO(test case): Not allowed - abstract static
    // abstract final void method() { }
    // TODO(test case): Not allowed - static final
    // static final void method() { }

    // TODO(test case): Not allowed - constructor does not have name of class
    // method() {}
}


