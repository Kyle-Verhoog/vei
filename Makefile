.PHONY: generate-executable
	
generate-executable:
	sbt clean
	sbt package
