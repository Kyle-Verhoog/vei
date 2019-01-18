# How to run
Run `make` to compile and `./joosc` to run the compiler

# Manual procedures
We rely on a serialization of the DFA we are going to use. To create the DFA you need to read in a .lex file, use Scanner.fromConfig(tokensDefn) on that file, and
then you need to run Scanner.serializeDfa() on that dfa

The grammar is defined in the grammar.json file, make any edits here
After this run the parseGrammarJson.rb file on this json file to create the .cfg file
Once you have a .cfg file you can run the Jlalr1.parse() function on this file to create the parse table, put this in a .lr1 file
You can use the Parser.readInLr1() function to read this in and create a CFG object that is used to actually perform parsing
