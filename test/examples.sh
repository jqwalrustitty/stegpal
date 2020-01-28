#!/bin/sh

defBMP="test-chimp.bmp"
defLorem="test-lorem.txt"

# ------------------------------------------------------------------------------

mkLorem() {
file=${1:-$defLorem}
echo "mkLorem: $file"
cat << LOREM > "$file"
Sed ut perspiciatis, unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam eaque ipsa, quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt, explicabo. Nemo enim ipsam voluptatem, quia voluptas sit, aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos, qui ratione voluptatem sequi nesciunt, neque porro quisquam est, qui dolorem ipsum, quia dolor sit amet consectetur adipiscing velit, sed quia non-numquam do eius modi tempora inciidunt, ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit, qui in ea voluptate velit esse, quam nihil molestiae consequatur, vel illum, qui dolorem eum fugiat, quo voluptas nulla pariatur?

At vero eos et accusamus et iusto odio dignissimos ducimus, qui blanditiis praesentium voluptatum deleniti atque corrupti, quos dolores et quas molestias excepturi sint, obcaecati cupiditate non-provident, similique sunt in culpa, qui officia deserunt mollitia animi, id est laborum et dolorum fuga.
LOREM
}

# ------------------------------------------------------------------------------

mkBMP() {
file=${1:-$defBMP}
echo "mkBMP: $file"
cat << HERE | base64 -d > "$file"
Qk16MAAAAAAAAHoAAABsAAAAQAAAAEAAAAABABgAAAAAAAAwAADEDgAAxA4AAAAAAAAAAAAAQkdS
cwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAIAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAD
AwcDAwcAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAADAwcDAwcAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAACAgsCAgsAAgAAAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAgsCAgsAAgAAAgAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
GacAGacoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAGacAGacoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgo
JtgoJtgoJtgoJtgoJtgoJtgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAGacAGacoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgo
JtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAGacAGacoJtgoJtgo
JtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgo
JtgoJtgoJtgoJtgoJtgoJtgoJtgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAGacA
GacoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJthroulroumMyf+Myf+Kxv+Kxv+L
yP+LyP+Iwv+Iwv8lULolULooJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAGacAGacoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgo
JthroulroumMyf+Myf+Kxv+Kxv+LyP+LyP+Iwv+Iwv8lULolULooJtgoJtgoJtgoJtgoJtgoJtgo
JtgoJtgoJtgoJtgoJtgoJtgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtiK
xv+Kxv+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+H
w/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/8cSLQcSLQoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAoJtgo
JtgoJtgoJtgoJtgoJtgoJtgoJtiKxv+Kxv+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+H
w/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/8cSLQcSLQoJtgoJtgoJtgo
JtgoJtgoJtgoJtgoJtgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAoJtgoJtgoJtgoJtgoJtgoJthmnOVmnOWHw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+H
w/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+H
w/+Hw/+Hw/+Hw/+Hw/+Hw/+Iwv+Iwv8oJtgoJtgoJtgoJtgoJtgoJtgAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAoJtgoJtgoJtgoJtgoJtgoJthmnOVmnOWHw/+Hw/+H
w/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+H
w/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Iwv+Iwv8oJtgoJtgoJtgoJtgo
JtgoJtgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAoJtgoJtiHw/+H
w/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/97tfd7tfcoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgo
JtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtg/bcg/bciHw/+H
w/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/9roulroukoJtgoJtgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAoJtgoJtiHw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/97tfd7tfcoJtgoJtgoJtgo
JtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgo
JtgoJtgoJtgoJtg/bcg/bciHw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/9roulroukoJtgoJtgAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAGacAGaeHw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/8o
JtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJthNftNNftOLyP+LyP+Hw/+Hw/+Hw/+Hw/+Kxv+K
xv97tfd7tfcoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtiHw/+Hw/+Hw/+Hw/+H
w/+Hw/+Hw/+Hw/8oJtgoJtgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAGacAGaeHw/+H
w/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/8oJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJthNftNNftOL
yP+LyP+Hw/+Hw/+Hw/+Hw/+Kxv+Kxv97tfd7tfcoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgo
JtgoJtgoJtiHw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/8oJtgoJtgAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAoJtgoJtiHw/+Hw/+Hw/+Hw/+Hw/+Hw/8oJtgoJtgoJtgoJtg/bcg/bciHw/+H
w/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+H
w/+Hw/+Hw/+Hw/+Iwv+Iwv8oJtgoJtgoJtgoJtgoJtgoJtiHw/+Hw/+Hw/+Hw/+Hw/+Hw/8oJtgo
JtgAGacAGacAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAoJtgoJtiHw/+Hw/+Hw/+Hw/+Hw/+Hw/8o
JtgoJtgoJtgoJtg/bcg/bciHw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+H
w/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Iwv+Iwv8oJtgoJtgoJtgoJtgoJtgoJtiH
w/+Hw/+Hw/+Hw/+Hw/+Hw/8oJtgoJtgAGacAGacAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAoJtgo
JtiHw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+H
w/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+H
w/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/8cSLQcSLQoJtgoJtgAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAoJtgoJtiHw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+H
w/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+H
w/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+H
w/8cSLQcSLQoJtgoJtgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAoJtgoJtiHw/+Hw/+Hw/+Hw/+H
w/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+H
w/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+H
w/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/8oJtgoJtgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAoJtgoJtiHw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+H
w/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+H
w/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/8oJtgoJtgAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAoJtgoJtgoJtgoJtiHw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+H
w/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+H
w/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+H
w/+Ixv+Ixv8oJtgoJtgoJtgoJtgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAoJtgoJtgoJtgoJtiH
w/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+H
w/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+H
w/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Ixv+Ixv8oJtgoJtgoJtgoJtgAAAAAAAAAAAAAAAAAAAAA
AAAoJtgoJtiHw/+Hw/9roulroukoJtgoJtgoJtgoJtgoJtgoJtiHw/+Hw/+Hw/+Hw/+Hw/+Hw/+H
w/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+LyP+LyP9fkt9fkt9roulroumIwv+Iwv+Hw/+Hw/+Hw/+H
w/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Kxv+Kxv8oJtgoJtgoJtgoJtgoJtgoJtiIwv+Iwv9r
oulroukoJtgoJtgAAAAAAAAAAAAAAAAoJtgoJtiHw/+Hw/9roulroukoJtgoJtgoJtgoJtgoJtgo
JtiHw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+LyP+LyP9fkt9fkt9r
oulroumIwv+Iwv+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Kxv+Kxv8oJtgo
JtgoJtgoJtgoJtgoJtiIwv+Iwv9roulroukoJtgoJtgAAAAAAAAAAAAAAAAoJtgoJtiHw/+Hw/+H
w/+Hw/+Myf+Myf8oJtgoJtgoJtgoJtgoJtgoJtiKxv+Kxv+Hw/+Hw/+Gw/+Gw//s+v/s+v/s+v/s
+v+Yy/uYy/uHw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw//d8/3d8/3t+v/t+v/G5fzG5fyGw/+Gw/+H
w/+Hw/8oJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtiIxv+Ixv+Hw/+Hw/+Hw/+Hw/8oJtgoJtgAAAAA
AAAAAAAAAAAoJtgoJtiHw/+Hw/+Hw/+Hw/+Myf+Myf8oJtgoJtgoJtgoJtgoJtgoJtiKxv+Kxv+H
w/+Hw/+Gw/+Gw//s+v/s+v/s+v/s+v+Yy/uYy/uHw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw//d8/3d
8/3t+v/t+v/G5fzG5fyGw/+Gw/+Hw/+Hw/8oJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtiIxv+Ixv+H
w/+Hw/+Hw/+Hw/8oJtgoJtgAAAAAAAAAAAAAAAAoJtgoJtiIwv+Iwv+Hw/+Hw/+Hw/+Hw/9Sg9ZS
g9YoJtgoJtgoJtgoJtiHw/+Hw/+Hw/+Hw//s+v/s+v/s+v/s+v/s+v/s+v/s+v/s+v/s+v/s+v+H
w/+Hw/+Hw/+Hw//s+v/s+v/s+v/s+v/s+v/s+v/s+v/s+v/t+v/t+v+Hw/+Hw/+Hw/+Hw/8oJtgo
JtgoJtgoJtiHw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/8oJtgoJtgAAAAAAAAAAAAAAAAoJtgoJtiI
wv+Iwv+Hw/+Hw/+Hw/+Hw/9Sg9ZSg9YoJtgoJtgoJtgoJtiHw/+Hw/+Hw/+Hw//s+v/s+v/s+v/s
+v/s+v/s+v/s+v/s+v/s+v/s+v+Hw/+Hw/+Hw/+Hw//s+v/s+v/s+v/s+v/s+v/s+v/s+v/s+v/t
+v/t+v+Hw/+Hw/+Hw/+Hw/8oJtgoJtgoJtgoJtiHw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/+Hw/8oJtgo
JtgAAAAAAAAAAAAAAAAoJtgoJth7tfd7tfeHw/+Hw/+Hw/+Hw/8oJtgoJtgoJtgoJtgoJtgoJtiH
w/+Hw/+Hw/+Hw//s+v/s+v/s+v/s+v/s+v/s+v/s+v/s+v/s+v/s+v/G5fzG5fzs+v/s+v/s+v/s
+v/s+v/s+v/s+v/s+v/s+v/s+v/s+v/s+v+Hw/+Hw/+Hw/+Hw/8oJtgoJtgoJtgoJtiMyf+Myf+H
w/+Hw/+Hw/+Hw/+Myf+Myf8oJtgoJtgAAAAAAAAAAAAAAAAoJtgoJth7tfd7tfeHw/+Hw/+Hw/+H
w/8oJtgoJtgoJtgoJtgoJtgoJtiHw/+Hw/+Hw/+Hw//s+v/s+v/s+v/s+v/s+v/s+v/s+v/s+v/s
+v/s+v/G5fzG5fzs+v/s+v/s+v/s+v/s+v/s+v/s+v/s+v/s+v/s+v/s+v/s+v+Hw/+Hw/+Hw/+H
w/8oJtgoJtgoJtgoJtiMyf+Myf+Hw/+Hw/+Hw/+Hw/+Myf+Myf8oJtgoJtgAAAAAAAAAAAAAAAAA
AAAAAAAoJtgoJtiHw/+Hw/+Hw/+Hw/8oJtgoJtgoJtgoJtgoJtgoJtiHw/+Hw/+Hw/+Hw//s+v/s
+v8AAAAAAAABARMBARPu9fvu9fvs+v/s+v/s+v/s+v/s+v/s+v/s+v/s+v8AAAAAAAABARMBARPd
5+3d5+3s+v/s+v+Hw/+Hw/+Hw/+Hw/8oJtgoJtgoJtgoJtiKxv+Kxv+Hw/+Hw/+Hw/+Hw/8oJtgo
JtgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAoJtgoJtiHw/+Hw/+Hw/+Hw/8oJtgoJtgoJtgoJtgo
JtgoJtiHw/+Hw/+Hw/+Hw//s+v/s+v8AAAAAAAABARMBARPu9fvu9fvs+v/s+v/s+v/s+v/s+v/s
+v/s+v/s+v8AAAAAAAABARMBARPd5+3d5+3s+v/s+v+Hw/+Hw/+Hw/+Hw/8oJtgoJtgoJtgoJtiK
xv+Kxv+Hw/+Hw/+Hw/+Hw/8oJtgoJtgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAoJtgoJtiHw/+H
w/+Hw/+Hw/+Ixv+Ixv8oJtgoJtgoJtgoJtiHw/+Hw/+Hw/+Hw//s+v/s+v8BARMBARPt+v/t+v8B
ARMBARPs+v/s+v/p+v/p+v/s+v/s+v/s+v/s+v8BARMBARO4w8e4w8cBARMBARPs+v/s+v+Hw/+H
w/+Hw/+Hw/8oJtgoJtgoJtgoJtiHw/+Hw/+Hw/+Hw/+Hw/+Hw/8oJtgoJtgAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAoJtgoJtiHw/+Hw/+Hw/+Hw/+Ixv+Ixv8oJtgoJtgoJtgoJtiHw/+Hw/+Hw/+H
w//s+v/s+v8BARMBARPt+v/t+v8BARMBARPs+v/s+v/p+v/p+v/s+v/s+v/s+v/s+v8BARMBARO4
w8e4w8cBARMBARPs+v/s+v+Hw/+Hw/+Hw/+Hw/8oJtgoJtgoJtgoJtiHw/+Hw/+Hw/+Hw/+Hw/+H
w/8oJtgoJtgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAGacAGacAGacAGacAGacAGacA
AAAAAAAoJtgoJtgoJtgoJtiHw/+Hw/+Gw/+Gw//s+v/s+v/s+v/s+v/t+v/t+v+Hw/+Hw/+Kxv+K
xv+Gw/+Gw//G5fzG5fzs+v/s+v/s+v/s+v/s+v/s+v+Hw/+Hw/8oJtgoJtgoJtgoJtgoJtgoJtgA
AAAAAAAoJtgoJtgoJtgoJtgoJtgoJtgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAGacAGacAGacAGacAGacAGacAAAAAAAAoJtgoJtgoJtgoJtiHw/+Hw/+Gw/+Gw//s+v/s+v/s
+v/s+v/t+v/t+v+Hw/+Hw/+Kxv+Kxv+Gw/+Gw//G5fzG5fzs+v/s+v/s+v/s+v/s+v/s+v+Hw/+H
w/8oJtgoJtgoJtgoJtgoJtgoJtgAAAAAAAAoJtgoJtgoJtgoJtgoJtgoJtgAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABCXbRCXbQoJtgo
JtgoJtgoJtiKxv+Kxv+Hw/+Hw/+Hw/+Hw/+LyP+LyP8oJtgoJtgoJtgoJtgoJtgoJth7tfd7tfeH
w/+Hw/+Iwv+Iwv+Hw/+Hw/8cSLQcSLQoJtgoJtgoJtgoJtgAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAABCXbRCXbQoJtgoJtgoJtgoJtiKxv+Kxv+Hw/+Hw/+Hw/+Hw/+LyP+LyP8oJtgo
JtgoJtgoJtgoJtgoJth7tfd7tfeHw/+Hw/+Iwv+Iwv+Hw/+Hw/8cSLQcSLQoJtgoJtgoJtgoJtgA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAoJtgoJtgoJtgoJtgo
JtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgo
JtgoJtgoJtgoJtgoJtgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgo
JtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAoJtgoJtgoJtgoJtgoJtgoJtgoJtgo
JtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAo
JtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgo
JtgoJtgoJtgoJtgoJtgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAoJtgoJtgoJtgoJtgoJtgoJtgo
JtgoJtgoJtgoJtgAGacAGacAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgoJtgAGacAGacAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=
HERE
}

# ------------------------------------------------------------------------------

doDemo() {

payload=${1:-$defLorem}
bmp_orig=${1:-$defBMP}

echo "PAYLOAD: $payload"
echo "DEFBMP:  $bmp_orig"

stegpal="../.cabal-sandbox/bin/stegpal"

base="temp"
bmp16=$base-16x16
bmp32=$base-32x32
bmp64=$base-64x64
favicon="favicon"

set -x
#rm -f "$base*.bmp" "$favicon*" out.dat "$defLorem" "$defBMP"

set +x
echo ""
echo "> --------------------------------------"
echo "Convert a bitmap and create an icon"
echo "> --------------------------------------"
set -x
convert "$bmp_orig" -scale 16x16 -type Palette -colorspace sRGB -depth 8 -colors 33 -compress none "$bmp16.bmp"
convert "$bmp_orig" -scale 32x32 -type Palette -colorspace sRGB -depth 8 -colors 33 -compress none "$bmp32.bmp"
convert "$bmp_orig" -scale 64x64 -type Palette -colorspace sRGB -depth 8 -colors 33 -compress none "$bmp64.bmp"
convert "$bmp16.bmp" "$bmp32.bmp" "$bmp64.bmp" "$favicon.ico"


set +x
echo ""
echo "> --------------------------------------"
echo "> Inject into $bmp16.bmp"
echo "> --------------------------------------"
set -x
$stegpal analyse -v -h "$bmp16.bmp"
$stegpal analyse -v "$bmp16.bmp"
$stegpal inject  -v -p "$payload" "$bmp16.bmp"
$stegpal analyse -v "$bmp16-steg.bmp"


set +x
echo ""
echo "> --------------------------------------"
echo "> Inject into $favicon.ico"
echo "> --------------------------------------"
set -x
$stegpal analyse -v -h "$favicon.ico"
$stegpal inject  -v -p "$favicon.ico" -o "$favicon-steg.ico" "$favicon.ico"
$stegpal analyse -v "$favicon-steg.ico"


set +x
echo ""
echo "> --------------------------------------"
echo "> Extract from $favicon-steg.ico and check md5sum"
echo "> --------------------------------------"
set -x
$stegpal extract -v -o out.dat "$favicon-steg.ico"
md5sum "$favicon.ico" out.dat 


set +x
echo ""
echo "> --------------------------------------"
echo "> Extract from $bmp16-steg.bmp"
echo "> --------------------------------------"
set -x
$stegpal extract -v "$bmp16-steg.bmp"


set +x
echo ""
echo "> --------------------------------------"
echo "> Injection/Extraction with keys"
echo "> --------------------------------------"
set -x
$stegpal inject  -v -k qweasd123 -p "$payload" "$bmp32.bmp"
$stegpal extract -v -k badpasswd "$bmp32-steg.bmp"
$stegpal extract -v -k qweasd123 "$bmp32-steg.bmp"


set +x
echo ""
echo "> --------------------------------------"

}

# ------------------------------------------------------------------------------
# Main

rm -f "$base*.bmp" "$favicon*" out.dat "$defLorem" "$defBMP"

mkLorem "$1"
mkBMP "$2"

doDemo "$1" "$2"


# ------------------------------------------------------------------------------
# vi: et ts=4 ai
