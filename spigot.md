## 基于流的 Spigot 算法的进位界估计

记有理数 $C=\dfrac{P}{Q}$，$\lambda=\dfrac{p}{2q}$. 考虑级数
$$
\begin{align}
S&=\frac{P}{Q}\sum_{k=0}^{\infty}{\frac{k!}{(2k+1)!!}\left(\frac{p}{q}\right)^k} \\
&=\frac{P}{Q}\bigg(1+\frac{p}{3q}\bigg(1+\frac{2p}{5q}\bigg(1+\cdots
\frac{kp}{(2k+1)q}\bigg(1+\cdots\bigg)\cdots\bigg)\bigg)\bigg),
\end{align}
$$
其前 $N$ 项之和为
$$
\begin{align}
S[N]&=\frac{P}{Q}\sum_{k=0}^{N-1}{\frac{k!}{(2k+1)!!}\left(\frac{p}{q}\right)^k} \\
&=\frac{P}{Q}\bigg(1+\frac{p}{3q}\bigg(1+\frac{2p}{5q}\bigg(1+\cdots
\frac{(N-1)p}{(2N-1)q}\bigg(1+\frac{Np}{(2N+1)q}\times 0\bigg)\cdots\bigg)\bigg)\bigg).
\end{align}
$$
第一截断误差
$$
\begin{align}
\Pi_1(N)&=S-S[N] \\
&=\frac{P}{Q}\cdot\frac{p}{3q}\cdot\frac{2p}{5q}\cdot\cdots\cdot\frac{Np}{(2N+1)q}\bigg(1+\cdots\bigg) \\
&\leqslant\frac{P}{Q}\left(\frac{p}{2q}\right)^N\left(1+\frac{p}{2q}+(\frac{p}{2q})^2+\cdots\right) \\
&=\frac{2qP}{(2q-p)Q}\left(\frac{p}{2q}\right)^N=\frac{C\lambda^N}{1-\lambda},
\end{align}
$$
标准型第二截断误差
$$
\begin{align}
\Pi_2&=\frac{1}{Q}\bigg((Q-1)+\frac{p}{3q}\bigg((3q-1)+\frac{2p}{5q}\bigg((5q-1)+\cdots\\
&\phantom{{}={}}\frac{kp}{(2k+1)q}\bigg(((2k+1)q-1)+\cdots\bigg)\cdots\bigg)\bigg)\bigg) \\
&\leqslant\frac{1}{Q}\bigg((Q-1)+\frac{p}{3q}\bigg(3q+\frac{2p}{5q}\bigg(5q+\cdots
\frac{kp}{(2k+1)q}\bigg((2k+1)q+\cdots\bigg)\cdots\bigg)\bigg)\bigg) \\
&\leqslant\frac{1}{Q}\bigg((Q-1)+p\cdot\bigg(1+\frac{2p}{3q}\bigg(1+\cdots
\frac{kp}{(2k-1)q}\bigg(1+\cdots\bigg)\cdots\bigg)\bigg)\bigg) \\
&\leqslant\frac{1}{Q}\bigg((Q-1)+p\cdot\bigg(1+\frac{p}{2q}\times2+(\frac{p}{2q})^2\times3+\cdots\bigg)\bigg) \\
&=1+\frac1Q\left(\frac{4pq^2}{(2q-p)^2}-1\right)=1+\frac1Q\left(\frac{p}{(1-\lambda)^2}-1\right).
\end{align}
$$
当 $Q=p=q=1$ 时，上述
$$
\Pi_2=\frac13\bigg(2+\frac25\bigg(4+\frac37\bigg(6+\cdots\frac{k}{(2k+1)}\bigg(2k+\cdots\bigg)\cdots\bigg)\bigg)\bigg)
$$
可以直接计算出来：

首先将 $2$ 表为
$$
2=\frac13\bigg(2+\frac25\bigg(4+\frac37\bigg(6+\cdots\frac{N}{(2N+1)}\bigg(2N+(2N+2)\bigg)\cdots\bigg)\bigg)\bigg),
$$
两式相减得余项
$$
\begin{align}
\Pi_2-2&=\frac13\times\frac25\times\cdots\times\frac{N}{2N+1}\times\\
&\phantom{{}={}}\Bigg(\frac{N+1}{2N+3}\bigg((2N+2)+\frac{N+2}{2N+5}\bigg((2N+4)+\cdots\bigg)\bigg)-(2N+2)\Bigg),
\end{align}
$$
上式大括号部分记作 $R(N)$，那么
$$
|\Pi_2-2|\leqslant\frac{|R(N)|}{2^N},
$$
一方面，
$$
R(N)\leqslant\left((N+1)+\frac{N+2}{2}+\frac{N+3}{2^2}+\cdots\right)-(2N+2)=2,
$$
另一方面，
$$
\begin{align}
R(N)&\geqslant\left(\frac{N+1}{2N+3}+(\frac{N+1}{2N+3})^2+(\frac{N+1}{2N+3})^3+\cdots\right)\cdot(2N+2)-(2N+2)\\
&=-\frac{2N+2}{N+2}\geqslant-2,
\end{align}
$$
因此 $R(N)$ 有界，从而得到 $\Pi_2=2$.

下面使用混合进制基底 $\left[\dfrac{jp}{(2j+1)q}\right]_{j=1}^\infty$ 下 Horner 形式的表示，那么 $\Pi_2$ 可简记为
$$
\frac1Q\left[Q-1;\,3q-1,\,5q-1,\cdots,\,(2k+1)q-1,\cdots\right].
$$
初始项数为 $A$、递增项数为 $u$、进制基为 $B$ 的三角型第二截断误差
$$
\begin{align}
\Pi_2(A,\,u,\,B)&=\frac{1}{Q}\big[Q-1;\,3q-1,\,5q-1,\cdots,\,(2A-1)q-1,\\
&\phantom{{}=\frac{1}{Q}\big[Q-1;\,{}}((2A+1)q-1)B,\cdots,\,((2(A+u)-1)q-1)B,\\
&\phantom{{}=\frac{1}{Q}\big[Q-1;\,{}}((2(A+u)+1)q-1)B^2,\cdots,\,((2(A+2u)-1)q-1)B^2,\\
&\phantom{{}=\frac{1}{Q}\big[Q-1;\,{}}\cdots\cdots\\
&\phantom{{}=\frac{1}{Q}\big[Q-1;\,{}}((2(A+(k-1)u)+1)q-1)B^k,\cdots,\,((2(A+ku)-1)q-1)B^k,\cdots\cdots\big] \\
&\leqslant\frac{1}{Q}\bigg((Q-1)+p\cdot\big[1;\,2,\,3,\cdots,\,A-1,\,AB,\cdots,\,(A+u-1)B,\cdots\cdots\\
&\phantom{{}\leqslant\frac{1}{Q}\bigg((Q-1)+p\cdot\big[1;\,{}}(A+(k-1)u)B^k,\cdots,\,(A+ku-1)B^k,\cdots\cdots\big]\bigg) \\
&\leqslant\frac{1}{Q}\bigg((Q-1)+p\cdot\Big(1+2\lambda+3\lambda^2+\cdots+(A-1)\lambda^{A-2}+\\
&\phantom{{}\leqslant\frac{1}{Q}\bigg((Q-1)+p\cdot\Big({}}B\cdot\big(A\lambda^{A-1}+\cdots+(A+u-1)\lambda^{A+u-2}\big)+\\
&\phantom{{}\leqslant\frac{1}{Q}\bigg((Q-1)+p\cdot\Big({}}\cdots\cdots\\
&\phantom{{}\leqslant\frac{1}{Q}\bigg((Q-1)+p\cdot\Big({}}
B^k\cdot\big((A+(k-1)u)\lambda^{A+(k-1)u-1}+\cdots+(A+ku-1)\lambda^{A+ku-2}\big)+\cdots\cdots\Big)\bigg) \\
&\leqslant 1+\frac{1}{Q}\bigg(p\cdot\Big(\frac{1}{(1-\lambda)^2}+
\frac{B\lambda^{A}}{(1-\lambda)^2(1-B\lambda^u)}+
\frac{B(A-u)\lambda^{A-1}}{(1-\lambda)(1-B\lambda^u)}+
\frac{Bu\lambda^{A-1}}{(1-\lambda)(1-B\lambda^u)^2}\Big)-1\bigg).
\end{align}
$$
按照计算要求，第一截断误差需要满足条件：对任意自然数 $k$，成立 $\Pi_1(A+ku)<B^{-k-1}$. 该条件等价于
$$
B\lambda^u<1,\;C\lambda^{A-u}<1-\lambda.
$$
我们取 $u=\left\lceil-\dfrac{\ln B}{\ln\lambda}\right\rceil+1$，那么成立 $B\lambda^u\leqslant\lambda$，在此情形下
$$
\Pi_2(A,\,u,\,B)\leqslant 1+\frac{1}{Q}\bigg(p\cdot\Big(\frac{1}{(1-\lambda)^2}+
\frac{\lambda^{A-u+1}}{(1-\lambda)^3}+
\max\left\{\frac{(A-u)\lambda^{A-u}}{(1-\lambda)^2},0\right\}+
\frac{u\lambda^{A-u}}{(1-\lambda)^3}\Big)-1\bigg).
$$
若 $C<1-\lambda$ 成立，我们取 $A=u$，则上式给出估计
$$
\Pi_2(A,\,u,\,B)\leqslant 1+\frac{1}{Q}\bigg(\frac{(1+u)p}{(1-\lambda)^3}-1\bigg);
$$
一般情况下，记 $n=A-u$，在 $C\lambda^n<1-\lambda$ 成立的前提下，我们寻找满足条件

$$
n+\frac{u+\lambda}{1-\lambda}<\lambda^{-n}
$$
的最小正整数 $n$​，可以采用 Newton 迭代法求函数
$$
F(x)=\lambda^{-x}-x-\frac{u+\lambda}{1-\lambda}
$$
的正零点：取初值 $x_0=1-\dfrac{\ln|\ln\lambda|}{|\ln\lambda|}$，使用公式
$$
\begin{align}
x_{n+1}&=x_n-\frac{F(x_n)}{F'(x_n)} \\
&=x_n-\frac{\lambda^{-x_n}-x_n-\dfrac{u+\lambda}{1-\lambda}}{\lambda^{-x_n}|\ln\lambda|-1} \\
&=\frac{\lambda^{-x_n}(x_n|\ln\lambda|-1)+\dfrac{u+\lambda}{1-\lambda}}{\lambda^{-x_n}|\ln\lambda|-1}
\end{align}
$$
进行迭代，则上式可以给出估计
$$
\Pi_2(A,\,u,\,B)\leqslant 1+\frac{1}{Q}\bigg(\frac{2p}{(1-\lambda)^2}-1\bigg).
$$
在此策略下，每一轮计算都将第一截断误差控制在 $1$ 以内，因此进位上界就是 $1+\lceil\Pi_2\rceil$.