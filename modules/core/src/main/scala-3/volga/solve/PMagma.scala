package volga.solve

trait PMagma[A]:
    def empty: A

    extension (x: A)
        def combine(y: A): A
        final def ##(y: A) = x.combine(y)

