#import "./judgement/meta.typ" as meta;
#import "./judgement/obj.typ" as obj;


#let problem(Ctx, problem) = {
  $Ctx ⊢ #problem$
}

#let tele(Ctx, tele) = {
  $Ctx ⊢ #tele thick bold("tele")$
}

#let subst(Ctx1, subst, Ctx2) = {
  $Ctx1 ⊢ #subst arrow.r.double #Ctx2$
}

#let crecord(Ctx, problem, record, tele) = {
  $Ctx attach(⊢, br: #problem) #record : #tele$
}