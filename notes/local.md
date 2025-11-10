### Local and Partial

$$
\frac{
  \Gamma\vdash\Delta~\operatorname{Tel}\quad
  \Gamma\Delta\vdash T
}{
  \Gamma\vdash \operatorname{Local}\; \Delta \; T
}
$$

$$
\frac{
  \Gamma\vdash\Delta~\operatorname{Tel}\quad
  \Gamma\Delta\vdash T\quad
  \Gamma\vdash\Sigma~\operatorname{Tel}\quad
  \Gamma\vdash\delta : \Sigma  \Rightarrow \Delta \quad
  \Gamma\Sigma\vdash t : T[\delta]
}{
  \Gamma\vdash \operatorname{partial}~\delta~\Sigma~t : \operatorname{Local}\; \Delta \; T
}
$$