library(MyRL)
print.state <- F
#' step size ¥sim 1/i => very slow convergence
#' but if we set sqrt(1/i), then we get rapid convergence
#' 素朴にはじっとしていると,コスト関数分だけValueFunctionが悪化する.
#' 従って, 周りのValueFunctionが良い状態への遷移が促される.
#' しかし, ValueFunctionの更新具合が遅くなりすぎると,なかなか動かなくなる,ということ.
#' 1つめ:なかなか動かなくなる
x <- Taxi1D(N=1,AS=c(-1,0,1), Cost=Taxi1DCost2, print.state=print.state)
x
#' 更新係数の関数をlog(1/x)にすることで速くなる
x <- Taxi1D(N=100,AS=c(-1,0,1), step.size.fun=function(x){1/sqrt(x)},Cost=Taxi1DCost2, print.state=print.state)
x
#' 更新係数の関数を定数にしてみると
x <- Taxi1D(N=100,AS=c(-1,0,1), step.size.fun=function(x){0.9},Cost=Taxi1DCost2, print.state=print.state)
x
x <- Taxi1D(N=100,AS=c(-1,0,1), step.size.fun=function(x){1/100},Cost=Taxi1DCost2, print.state=print.state)
x

#' 障害を乗り越える
#' コスト関数に障害を反映させておかないといつまでたってもだめ。
#' 探索ポリシーを交ぜると,コスト関数に工夫をしなくてもいけるかもしれない...?
x <- Taxi1D(N=1000, AS=c(-1,0,1,2), step.size.fun=function(x){sqrt(1/x)},Transition=Taxi1DTransition, Cost=Taxi1DCost2Revised,print.state=print.state)
x

#' ランダムな風を使う
#' a > 0 のとき, 負の方向に1だけランダムに戻る
#' a <= 0 の時, 正の方向に1だけランダムに進む
x <- Taxi1D(N=1000, AS=c(-1,0,1,2), step.size.fun=function(x){sqrt(1/x)},Transition=function(ss,a,L){Taxi1DTransition2(ss=ss,a=a,L=L,prob=0.1)} ,Cost=Taxi1DCost,print.state=print.state)
x
#' Costを距離の2乗に比例する形にして
#' 確率を変えていく
x <- Taxi1D(N=1000, AS=c(-1,0,1,2), step.size.fun=function(x){sqrt(1/x)},Transition=function(ss,a,L){Taxi1DTransition2(ss=ss,a=a,L=L,prob=0.1)} ,Cost=Taxi1DCost2,print.state=print.state)
x
x <- Taxi1D(N=1000, AS=c(-1,0,1,2), step.size.fun=function(x){sqrt(1/x)},Transition=function(ss,a,L){Taxi1DTransition2(ss=ss,a=a,L=L,prob=0.3)} ,Cost=Taxi1DCost2,print.state=print.state)
x
#' big L
x <- Taxi1D(L=1001, N=1000, AS=c(-1,0,1,2), step.size.fun=function(x){sqrt(1/x)},Transition=function(ss,a,L){Taxi1DTransition2(ss=ss,a=a,L=L,prob=0.1)} ,Cost=Taxi1DCost2,print.state=print.state)
x
