# pi
Calculating PI with infinite precision in scheme language.

The algorithm is based on the formula
<!--
$$
\arctan t=\frac{t}{1+t^2}
\sum_{n=0}^{\infty}{\left[
  \frac{n!}{(2n+1)!!}
  \left(\frac{2t^2}{1+t^2}\right)^n
\right]}
$$
-->
![image](https://github.com/Matheritasiv/pi/raw/main/formula.svg)

Run with Chez Scheme

## License
[WTFPL](http://www.wtfpl.net/txt/copying)
