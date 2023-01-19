# Volga

| Release | 
| --- |
| [![Maven Central](https://img.shields.io/maven-central/v/org.manatki/volga-macros_2.13.svg)](https://search.maven.org/search?q=org.manatki.volga) | 

Arrow and Symmetric Monoidal Category composition syntax helper

## How to use:
Let's say you have ` type Process[Input, Output]`, you want to use process comprehensions for your type

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
    val someProcess1: Process[Pair[A,B], Pair[X, Y]] = ...
    val someProcess2: Process[Pair[C, X], Z] = ...
    val someProcess3: Process[One, D] = ...
    val someProcess4: Process[Y, One] = ...
    val someProcess5: Process[Z, E] = ...
   
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

1. a comprehension for arrow

    ```scala
    proc: (X1, X2, ...) -> (Y1, Y2, ...)
    ```
    or monoidal morphism (note left associativity of products)
    ```scala
    proc: (Tensor(...(Tensor(X1, X2), ...) -> Tensor(...Tensor(Y1, Y2),..)
    ``` 
    should form a lambda function having parameters of types `(V[X1], V[X2], ...) =>`

2. last line in comprenension defined in (1.)

    should be in the form `(y1, y2, ...)` 
    or non-assigning application
    `someProcess(z1, z2, ...)` where `someProcess` is an arrow
    ```scala
    proc: (Z1, Z2, ...) -> (Y1, Y2, ...)
    ```
    or monoidal morphism (note left associativity of products)
    ```scala
    proc: (Tensor(...(Tensor(Z1, Z2), ...) -> Tensor(...Tensor(Y1, Y2),..)
    ``` 
  
3. to use some arrow  

    ```scala
    proc: (X1, X2, ...) -> (Y1, Y2, ...)
    ```
    or monoidal morphism (note left associativity of products)
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
    could be used to dispose of the result of an arrow, or use morphism with unit domain

5. special block-separator

    ```scala
     ----
     ```
     could be used to enforce the end of parallel block

## Additional rules for symmetric monoidal categories

1.  Any variable defined in the lambda parameter clause 
or extracted as a result of morphism application should be used **exactly one time**


## Limitations.

Volga syntax works by extending 
your arrows and morphisms with the `apply`
methods which then are analyzed by the macros.

Types which have `apply` method already
i.e. `Function1` and `cats.data.Kleisli` 
may not work. 
Consider volga.data.Kleisli instead.
