namespace Shared

module xString =
    let split (s: char) (x: string) = x.Split s

module xSeq =
    let mapInner (f: 'a -> 'b) (x: 'a seq seq) = Seq.map (Seq.map f) x
