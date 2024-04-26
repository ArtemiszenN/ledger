val data_path : string
val create_data_if_not_exist : unit -> Base.unit

val load :
  unit ->
  (Base.String.t, Base.String.comparator_witness) Base.Set.t
  * ( Stringpair.StringPair.t,
      Money.t,
      Stringpair.StringPair.comparator_witness )
    Base.Map.t

val save :
  (Base.String.t, 'a) Base.Set.t ->
  (Stringpair.StringPair.t, Money.t, 'b) Base.Map.t ->
  unit
