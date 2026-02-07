#import "../styles/lib.typ": *

#todo([motivation])

Otus employs a two-level type system, consisting of a meta-level and an object-level.  

#todo([inner type])

#todo([outer type and its motivation])

The outer language is design to glue together multiple object-level programs while deciding their meta variables dynamically. 
Each open object-level term can be lifted into outer-level together with its context. 
The outer-level language provides machinisms to manipulate inner-level programs while guaranteeing type safety, such as adding new assumptions to the context or applying inner-level functions.
