#!/usr/bin/env ruby
require("JSON")

json = JSON.parse(File.read(ARGV[0]))

startingSymbol = json['startingSymbol']
terminals = json['terminals']
nonterminals = json['nonterminals']
prodRules = json['productionRules']

if (prodRules.keys - nonterminals).count > 0 or (nonterminals - prodRules.keys).count > 0
  puts "Warning: number of proudction rules differs from nonterminals!"
  exit
end

if (prodRules.keys - nonterminals).count > 0
  puts "Warning: number of proudction rules differs from nonterminals!"
  exit
end

prodRulesDefn = []
prodRules.keys.each { |lhs| prodRules[lhs].each { |rhs| prodRulesDefn << "#{lhs} #{rhs}"} }

puts terminals.count
puts terminals
puts nonterminals.count
puts nonterminals
puts startingSymbol
puts prodRulesDefn.count
puts prodRulesDefn
