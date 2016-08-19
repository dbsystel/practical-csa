# Practical Connection Scan Algorithm Implementation

> _Under construction!_

This is a high level implementation of the Connection Scan Algorithm.
Modifications are made to fit special requirements for timetable information systems at [Deutsche Bahn](http://deutschebahn.com).
Goal of this implementation is to find out about difficulties when applying the algorithm to real world problems and requirements and to provide a publicly available working example.

## About the algorithm

The algorithm was introduced in 2013 by [Julian Dibbelt, Thomas Pajor, Ben Strasser, and Dorothea Wagner](http://i11www.iti.uni-karlsruhe.de/extra/publications/dpsw-isftr-13.pdf).

## About the project

My bachelor thesis studies the relevance and applicability of the algorithm for [Deutsche Bahn](http://deutschebahn.com).
This repository contains code that was created for the thesis.

## Getting started

To run this project you will need:

| Programm                            | Version |
| :---------------------------------- | :------ |
| [Scala](http://www.scala-lang.org/) | > 2.11  |
| [JDK](http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html) | > 8 |
| [SBT](http://www.scala-sbt.org/)    | > 0.13  |

Clone or download the files from Github.

Get the [inofficial Fernverkehr GTFS Feed](https://github.com/fredlockheed/db-fv-gtfs/). For now this is hardcoded to the fields provided in this feed. If you want to use your own GTFS feed take a look into [gtfs.readers](https://github.com/dbsystel/practical-csa/blob/master/src/main/scala/gtfs/readers.scala) and make sure the parameters are matched correctly to the case class fields of the type.

Use you command line to execute the following commands in the project's folder:

```
sbt compile
sbt run
```

Enter the location of the extracted GTFS feed on you file system. This will load the data and start a HTTP REST Endpoint on `http://localhost:8080`.

Query connections with:

```
get http://localhost:8080/query/{from}/{to}
```

With `from` and `to` being the integer stop IDs of your GTFS feed.

## Licence

Will follow soon...
