
type value =
  | PList of value list
  | PInt of int

type packet = value list

type packet_pair = packet * packet
