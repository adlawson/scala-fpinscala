# Functional Programming in Scala

These are my completed exercises from the book [Functional Programming in
Scala](http://www.amazon.co.uk/Functional-Programming-Scala-Paul-Chiusano/dp/1617290653)
by Paul Chiusano and Rúnar Bjarnason.

## Exercises
- [Chapter 2, Getting started](src/chapter02)
- [Chapter 3, Data structures](src/chapter03)
- Chapter 4, Error handling
- [Chapter 5, Strictness and laziness](src/chapter05)
- [Chapter 6, Functional state](src/chapter06)

## Running tests
```bash
$ curl -O https://raw.githubusercontent.com/adlawson/vagrantfiles/master/scala/Vagrantfile
$ vagrant up
$ vagrant ssh
...

$ cd /srv
$ sbt test
```
