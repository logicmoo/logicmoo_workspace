% n=0
def(r00).
head(r00, p0).

def(r01).
head(r01, q0).

% n=1
head(r10, p1).
body(r10, p0).

head(r11, p1).
body(r11, q0).

% n=2
head(r20, p2).
body(r20, p1).
body(r20, p0).

head(r21, p2).
body(r21, p1).
body(r21, q0).

% n=3
head(r30, p3).
body(r30, p2).
body(r30, p0).

head(r31, p3).
body(r31, p2).
body(r31, q0).

