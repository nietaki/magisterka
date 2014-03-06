# Apache Spark, Decision Trees and streaming data, all in one place #

# help #
to compile and run your code:

`sbt package
sbt run`

to run a specific main class with arguments:

`sbt "run-main SimpleApp arg"`


To generate the intelliJ project:

`sbt gen-idea`

To start the master:

`./sbin/start-master.sh`

point browser to localhost:8080 to get the master url

`./sbin/start-slave.sh 1 spark://xebab:7077`



## Why is the spark dependency in build.sbt commented out? ##
If you use spark downloaded from the repositories, you have to make sure you are using the same version (the same .jar)
in the workers. That makes things a little more complicated when you're running a standalone cluster - you have the .jar
without all the goodies that come with the source from git like the worker startup scripts. That's why it might be a
better option to compile spark locally and copy the generated jar, in my case

`~/incubator-spark/assembly/target/scala-2.10/spark-assembly-0.9.0-incubating-SNAPSHOT-hadoop1.0.4.jar`

to the lib/ directory.

If you plan on running the tests only locally, the repositories are a good choice.

