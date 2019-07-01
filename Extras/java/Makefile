#   Makefile for Java Multicopter class
#
#   Copyright(C) 2019 Simon D.Levy
#
#   MIT License

DIR = edu/wlu/cs/levy/multicopter
SRC = $(DIR)/Multicopter.java

MulticopterTest.class: MulticopterTest.java multicopter.jar
	javac -cp multicopter.jar MulticopterTest.java

multicopter.jar: $(SRC)
	javac $(SRC)
	jar cvf multicopter.jar edu

test: MulticopterTest.class multicopter.jar
	java -cp multicopter.jar:. MulticopterTest

doc:
	javadoc -d doc $(SRC)

clean:
	rm -f *.jar *.class $(DIR)/*.class *~
