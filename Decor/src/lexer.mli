type line = int
type col = int
type start = line * col
type stop = line * col
type interval = start * stop

val find_candidates : src:string -> out:string -> interval list
