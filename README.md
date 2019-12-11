# Volga

| Release | 
| --- |
| [![Maven Central](https://img.shields.io/maven-central/v/org.manatki/volga-macros_2.13.svg)](https://search.maven.org/search?q=org.manatki.volga) | 

Arrow and Symmetric Monoidal Category composition syntax helper

## How to use:
Lets say you have ` type Process[Input, Output]`, you want to use process comprehensions for your type

1. Define volga.Arr or volga.Symon instance for your type
2. Import syntax extensions
    ```scala
    import volga.syntax.comp._
    import volga.syntax.cat._
    import volga.syntax.symon._ 
    // or import volga.syntax.arr._
    ```
3. Prepare syntax composer for your type
    ```scala
        val process = arr[Process]
    ```
    or
    ```scala
        val process = symon[Process, Pair, One]
    ```
    where `Pair` and `One` are tensor product type constructor and unit type for your Symon instance
4. write your arrows
    ```scala
    val myProcess: Process[Pair[Pair[A, B], C], Pair[D, E]] = process{ (a, b, c) =>
        val (x, y) = someProcess1(a, b)
        val z = someProcess2(c, x)
        ----
        val d = someProcess3()
        someProcess4(y)
        val e = someProcess5(z)
        (d, e)
    }
    ```

## Syntactic rules

1. comprehension for arrow

    ```scala
    proc: (X1, X2, ...) -> (Y1, Y2, ...)
    ```
    or monoidal morpshism (note left assiciativity of products)
    ```scala
    proc: (Tensor(...(Tensor(X1, X2), ...) -> Tensor(...Tensor(Y1, Y2),..)
    ``` 
    should form a lambda function having parameters of types `(V[X1], V[X2], ...) =>`

2. last line in comprenension defined in 1. 

    should be in form `(y1, y2, ...)` 
    or non-assigning application
    `someProcess(z1, z2, ...)` where `someProcess` is an arrow
    ```scala
    proc: (Z1, Z2, ...) -> (Y1, Y2, ...)
    ```
    or monoidal morpshism (note left assiciativity of products)
    ```scala
    proc: (Tensor(...(Tensor(Z1, Z2), ...) -> Tensor(...Tensor(Y1, Y2),..)
    ``` 
  
3. to use some arrow  

    ```scala
    proc: (X1, X2, ...) -> (Y1, Y2, ...)
    ```
    or monoidal morpshim (note left assiciativity of products)
    ```scala
    proc: (Tensor(...(Tensor(X1, X2), ...) -> Tensor(...Tensor(Y1, Y2),..)
    ``` 
    you can write
    ```
    val (y1, y2, ...) = proc(x1, x2, ...)
    ```
  
4. non-assigning construction

    ```scala
    proc(x1, x2, ...)
    ```
    could be used to dispose result of arrow, or use morphism with unit domain

5. special block-separator

    ```scala
     ----
     ```
     could be used to enforce the end of parallel block
