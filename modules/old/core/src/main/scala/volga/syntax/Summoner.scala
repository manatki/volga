package volga.syntax

trait Summoner[TC[p[_, _]]] {
    def apply[P[_, _]](implicit instance: TC[P]): TC[P] = instance
}
