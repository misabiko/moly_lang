// This file was generated by lezer-generator. You probably shouldn't edit it.
import {LRParser} from "@lezer/lr"
const spec_Identifier = {__proto__:null,let:36, true:42, false:44, return:46, fn:50, while:58}
export const parser = LRParser.deserialize({
  version: 14,
  states: "&`OYQPOOOOQO'#Cp'#CpOwQPO'#CpOOQO'#Cm'#CmOOQO'#Ch'#ChQYQPOOOOQO'#Cb'#CbO!YQPO'#C_OwQPO'#CeO!_QPO'#CfOwQPO'#CgO!gQPO,59[OOQO-E6f-E6fO!lQPO,58yOOQO,59P,59PO!qQPO,59QO!yQPO,59QO#OQPO,59ROOQO1G.v1G.vOwQPO1G.eO#TQPO'#CvO#YQPO1G.lO!qQPO1G.lO#_QPO1G.mOOQO7+$P7+$PO#fQPO,59bO#nQPO7+$WO#sQPO7+$WO#xQPO7+$XOOQO7+$X7+$XO$PQPO'#CiO$[QPO1G.|O$dQPO<<GrO$iQPO<<GrOOQO<<Gs<<GsO$nQPO,59TOOQO-E6g-E6gOOQOAN=^AN=^O$sQPOAN=^OOQO1G.o1G.oOOQOG22xG22x",
  stateData: "$x~O`OSPOS~OSPOTPOVQObVOeUOfUOgWOiXOmYO~OSPOTPOVQOeUOfUO~OS]O~OS`OV_O~OWbO~OccO~OSdOWjP~OVfO~OlgO~OSiO~OWjO~OhmO~PYOknOWja~OlpO~OWqO~OhrO~PYOSsOW]Xk]X~OknOWji~OhuO~OlvO~OSwO~OhxO~O",
  goto: "!|kPPPlPPrPPlll|!WPPP!^PP!dPPPPP!vXROTglaPOQTWYcglQTOS[TlRlgQoiRtoXSOTglWROTglQZQQ^WQaYRhcQe_Rkf",
  nodeNames: "⚠ LineComment Program Let Identifier String Boolean ( ) Return Function WhileStatement",
  maxTerm: 29,
  skippedNodes: [0,1],
  repeatNodeCount: 2,
  tokenData: "$d~R_XY!QYZ!Q]^!Qpq!Qrs!cxy#Qyz#V|}#[!P!Q#a!_!`#r!c!}#w#R#S#w#T#o#w#o#p$Y#q#r$_~!VS`~XY!QYZ!Q]^!Qpq!Q~!fTOr!crs!us#O!c#O#P!z#P~!c~!zOT~~!}PO~!c~#VOV~~#[OW~~#aOk~~#dP!P!Q#g~#lQP~OY#gZ~#g~#wOc~~#|SS~!Q![#w!c!}#w#R#S#w#T#o#w~$_Ol~~$dOh~",
  tokenizers: [0],
  topRules: {"Program":[0,2]},
  specialized: [{term: 4, get: value => spec_Identifier[value] || -1}],
  tokenPrec: 0
})
