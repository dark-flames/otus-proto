### Dynamic
$$
\frac{
  \Gamma\vdash^o\Delta~\operatorname{Tel}\quad
  \Gamma\Delta\vdash^o T
}{
  \Gamma\vdash^o \operatorname{Dynamic}\; \Delta \; T
}
$$

$$
\frac{
  \Gamma\vdash^o\Delta~\operatorname{Tel}\quad
  \Gamma\Delta\vdash^o T\quad
  \Gamma\vdash^o\delta : \circ  \Rightarrow \Delta \quad
  \Gamma\vdash t : T[\delta]
}{
  \Gamma\vdash^o \operatorname{ok}~\delta~t : \operatorname{Dynamic}\; \Delta \; T
}
$$

$$
\frac{
  \Gamma\vdash^o\Delta~\operatorname{Tel}\quad
  \Gamma\Delta\vdash^o T\quad
}{
  \Gamma\vdash^o \operatorname{error}: \operatorname{Dynamic}\; \Delta \; T
}
$$

$$
\frac{
  \Gamma\vdash^o t : \operatorname{Dynamic}~\Delta~T\quad
  \Gamma\Delta,T\vdash^o n : B
}{
  \Gamma\vdash^o \operatorname{dBind}~t~n : \operatorname{Dynamic}~(\Delta,T)~B
}
$$


### Local and Partial

$$
\frac{
  \Gamma\vdash^o\Delta~\operatorname{Tel}\quad
  \Gamma\Delta\vdash^m T
}{
  \Gamma\vdash^m \operatorname{Local}\; \Delta \; T
}
$$

$$
\frac{
  \Gamma\vdash^o\Delta~\operatorname{Tel}\quad
  \Gamma\Delta\vdash^m T\quad
  \Gamma\vdash^o\Sigma~\operatorname{Tel}\quad
  \Gamma\vdash^o\delta : \Sigma  \Rightarrow \Delta \quad
  \Gamma\Sigma\vdash^m t : T[\delta]
}{
  \Gamma\vdash^m \operatorname{partial}~\delta~\Sigma~t : \operatorname{Local}\; \Delta \; T
}
$$

$$
\frac{
  \Gamma\vdash^m t : \operatorname{Local}~\Delta~T\quad
  \Gamma\Delta,T\vdash^m n : B
}{
  \Gamma\vdash^i \operatorname{let~open}~t~\operatorname{in}~n : \operatorname{Dynamic}~(\Delta,T)~B
}
$$