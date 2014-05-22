#' step size ¥sim 1/i => very slow convergence
#' but if we set sqrt(1/i), then we get rapid convergence
#' 素朴にはじっとしていると,コスト関数分だけValueFunctionが悪化する.
#' 従って, 周りのValueFunctionが良い状態への遷移が促される.
#' しかし, ValueFunctionの更新具合が遅くなりすぎると,なかなか動かなくなる,ということ.
x <- Taxi1D(N=1,AS=c(-1,0,1), Cost=Taxi1DCost2, print.state=T)
x <- Taxi1D(N=1,AS=c(-1,0,1), step.size.fun=function(x){1/sqrt(x)},Cost=Taxi1DCost2, print.state=T)

#' 障害を乗り越える
#' コスト関数に障害を反映させておかないといつまでたってもだめ。
#' 探索ポリシーを交ぜると,コスト関数に工夫をしなくてもいけるかもしれない...?
x <- Taxi1D(N=1000, AS=c(-1,0,1,2), step.size.fun=function(x){sqrt(1/x)},Transition=Taxi1DTransition, Cost=Taxi1DCost2Revised,print.state=F)
