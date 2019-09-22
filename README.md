# Fasti

[![Build Status](https://travis-ci.org/dokwork/fasti.svg?branch=master)](https://travis-ci.org/dokwork/fasti)
[![Coverage Status](https://coveralls.io/repos/github/dokwork/fasti/badge.svg?branch=master)](https://coveralls.io/github/dokwork/fasti?branch=master)

![](fasti.gif) 

_In ancient Rome, the **fasti** were chronological or calendar-based lists, or other diachronic records or 
plans of official and religiously sanctioned events._

## About
The main idea of this project is to separate the implementation of Sagas pattern and the implementation of their persistence to give more ability to make different variants of persistence. 

`Core` project contains an implementation of the saga which can be easily described, invoked and in case of any exception - compensated.

`Persistence` project contains one of a possible implementation of the persistence.

An example of the persisted saga is described in the `example` project.

## More about
- Complete specification of the `Saga` [here](core/src/test/scala/ru/dokwork/fasti/SagaSpec.scala)
- Example of the persisted saga [here](example/README.md)