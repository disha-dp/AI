import ply

try:
    from pyparsing import (alphanums, alphas, delimitedList, Forward,nums,Optional,
            Group, Keyword, Literal, opAssoc, operatorPrecedence,
            ParserElement, ParseException, ParseSyntaxException, Suppress,
            Word)
except ImportError:
    from pyparsing_py3 import (alphanums, alphas, delimitedList, Forward,
            Group, Keyword, Literal, opAssoc, operatorPrecedence,
            ParserElement, ParseException, ParseSyntaxException, Suppress,
            Word)

'''
ident = Word(alphas)
num = Word(nums)
term = ident | num
func = ident + Optional(delimitedList(term))
print(func.parseString("fn a,b,100"))  # -> ['fn', 'a', 'b', '100']

func = ident + Group(Optional(delimitedList(term)))
print(func.parseString("fn a,b,100"))  # -> ['fn', ['a', 'b', '100']]
'''

'''
make it to read expressions with bracket as a single term
'''

print "--------------------------TODO: CHANGE CODE--------------------"

def actCombineWord(word):
    return ''.join(x for x in word)

def parse(text):
    #only_small_list = list(alphas)[:26]
    #only_small = ''.join(x for x in only_small_list) 
    #left_parenthesis, right_parenthesis, colon = map(Suppress, "():")
    left_parenthesis, right_parenthesis =  "(",")"
    
    implies = Literal("=>")
    or_ = Literal("|")
    and_ = Literal("&")
    not_ = Literal("~")
    symbol = (Optional(not_)+ Word(alphas) + left_parenthesis + delimitedList(Word(alphas)) + right_parenthesis).setParseAction(actCombineWord) #, alphanums) 
    #   agg_sym = symbol + Optional(left_parenthesis) + Optional(delimitedList(variable)) + Optional(delimitedList(symbol))  + Optional(right_parenthesis)# | symbol | right_parenthesis)
    term = Forward()
    #term << (Group(agg_sym + Group(left_parenthesis +
    #               delimitedList(term) + right_parenthesis)) | agg_sym)
    
    term << (Group(symbol + Group(left_parenthesis +
                   delimitedList(term) + right_parenthesis)) | symbol)


    formula = Forward()
    operand = term

    formula << operatorPrecedence(operand, [
                                  #(not_, 1, opAssoc.RIGHT),
                                  (and_, 2, opAssoc.LEFT),
                                  (or_, 2, opAssoc.LEFT),
                                  (implies, 2, opAssoc.RIGHT)])
    try:
        result = formula.parseString(text, parseAll=True)
        assert len(result) == 1
        return result[0].asList()
    except (ParseException, ParseSyntaxException) as err:
        print("Syntax error:\n{0.line}\n{1}^".format(err,
              " " * (err.column - 1)))
        return []

'''
- use understanding to get that there can be things as variables and constants
'''


#A(x) => H(x)
#print parse("(~A(x) & H(x))") #[['~', ['A', ['x']]], '&', ['H', ['x']]]

def resolveImplication(expr):
    print 'resolveImplication......', expr
    if '=>' in expr:
        idxImplies = expr.index('=>')
        leftPart = expr[:idxImplies]
        rightPart = expr[idxImplies+1:]
        
        newTerms =  ['~'] + [leftPart] + ['|'] + rightPart
        print 'newTerms: ....', newTerms

        return newTerms
        #listOfTerms =  ['~'] + leftPart + ['|'] + rightPart

        #print 'LOT:', listOfTerms 
        #listOfTerms = [listOfTerms]
        #return listOfTerms
    else:
        c = doCNFStep1(expr)

    return c

#newP = []
def doCNFStep1(listOfTerms):
    #global newP
    newP = []
    print 'doCNF....', listOfTerms
    if '=>' in listOfTerms:
        idxImplies = listOfTerms.index('=>')
        leftPart = listOfTerms[:idxImplies]
        rightPart = listOfTerms[idxImplies+1:]
        #check if needed to convert to a list
        listOfTerms =  ['~'] + leftPart + ['|'] + rightPart
        print ' LOT:', listOfTerms

    for term in listOfTerms:
        #print 'inp term list: -----', term

        if type(term) is list:
            res = resolveImplication(term)
            #print 'after implicaiton resolution op term list:----',res
            #print 'ressssss: ',res
            newP.append(res)

        elif term == '=>':
                term = '|'
                #print 'term instead of implies',term
                newP.append(term)
            
        else:
            #print 'term!:',term 
            newP.append(term)
            #newP.append(listOfTerms)

    return newP


def applyNegation(expr):
    if type(expr) is str:
        if expr[0] =='~':
            return expr[1:]
        return '~'+expr 

    print 'applyNegation.....'
    print expr
    prevTerm = None
    altExp = []

    for term in expr:
        print 'term to negate: ', term

        #if prevTerm == '~' and type(term) is list:
        #    print 'sub term apply negation', term
            
        #    term = applyNegation(term)
        #    print 'return from apply negation'


        if term == '~':
            prevTerm = '~' 
            continue

        if prevTerm == '~':
            prevTerm = term
            ret = applyNegation(term)
            ret2 = applyNegation(ret)
            altExp.append(ret2)
            continue

        if type(term) is list:
        #    term = doCNFStep2(term) 
            print 'apply negation on:....', term , altExp
            ret = applyNegation(term)
            print 'successfully applied neg on:....', term, 'resulting expr: ', ret
            print 'altExp before appending',altExp , 'term: ',term
            altExp.append(ret)
            print 'altExp after coming back from applyneg to a term list',altExp , 'term: ',term
            
            #term = term[0]
        else:
            copyTerm = term
            print 'term in applyNegation...', term
            if term[0] =='&' :
                term = '|'

            elif term[0] =='|' :
                term = '&'

            elif term[0] =='&' :
                term = '|'

            elif term[0]== '~':
                #print 'before neg removal:...',expr , term
                #expr.remove(term)
                #print 'after neg removal:...',expr 
                
                #term = term[1:] 

                continue

                #term =  term[1:]
            else: 
                print 'termi:', term
                term = '~' + term


            #if type(term) is list:
               #altExp.append(term[0])
            print 'appending.....', term , 'to: ', altExp
            altExp.append(term)
            print  'altexp: now: ', altExp

        prevTerm = term


    print 'terms after applying negation: ', altExp
    if len(altExp) == 1:
        altExp = altExp[0]
    return altExp

#move negation inside
def doCNFStep2(opCNF1):
    '''
    when you see a negation, move it inside the adjacent bracket if any
    '''
    opCNF2 = []
    #op =[]
    print 'doCNFStep2........'
    prevTerm = None
    for term in opCNF1:
        print '----------term: ', term,'type term: ', type(term),'pev term:---', prevTerm
        if prevTerm == '~' :#and type(term) is list:
            negated = applyNegation(term)
            print 'negated term!!!', negated , term
            #if len(negated) == 1:
            #    print 'neg with len1',negated
            #    negated = negated[0]
            opCNF2.append(negated)
            term = negated

        elif type(term) is list:
            op = doCNFStep2(term) 
            print '======blah blue'
            opCNF2.append(op)

        elif term != '~' :
            opCNF2.append(term)
        prevTerm = term
    return opCNF2

# if outer list operator is same as all the operators of the inner list, then join the items of the inner list to the outer list with the 
#common operand
def bracketOpen(expr):
    print 'bracketOpen for....' , expr
    finalTerms =[]
    outerListOperand = expr[1]
    lhs = expr[0]
    rhs = expr[2]
    print  'outerListOperand :  ',outerListOperand
    
    #if type(lhs) is not list and type(rhs) is not list :

    if type(lhs) is list:
        lInter = bracketOpen(lhs)
        print 'converted lhs using bOpen....', lInter
        innerLHSOper = lhs[1]
        if innerLHSOper == outerListOperand:
            for term in lInter:
                finalTerms.append( term )
        else:
            finalTerms.append(lInter)
        '''innerOPer = lhs[1]
        inList = []
        for lTerm in lhs:
            if type(lTerm) is list:
                lInter = bracketOpen(lTerm)
                if lInter[1] == innerOPer:
                    for t in lInter:
                        inList.append(t)
                else:
                    inList.append(lInter)
            else:
                inList.append(lTerm)
            

        #finalTerms.append(inList)
        lt2 = lhs[1]
        if lt2 == outerListOperand:
            for lTerm in inList:
                finalTerms.append(lTerm) 
            
            finalTerms.append(outerListOperand)
        else:
            finalTerms.append([inList])'''

    else:
        finalTerms.append(lhs)

    finalTerms.append(outerListOperand)

    if type(rhs) is list:
        rInter = bracketOpen(rhs)
        print 'converted rhs using bOpen....', rInter
        innerRHSOper = rInter[1]
        if innerRHSOper == outerListOperand:

            for term in rInter:
                finalTerms.append( term )
        else:
            finalTerms.append(rInter)
        #finalTerms.append( rInter )
        
        '''innerOPer = rhs[1]
        inList = []
        for rTerm in rhs:
            if type(rTerm) is list:
                rInter = bracketOpen(rTerm)
                if rInter[1] == innerOPer:
                    for t in rInter:
                        inList.append(t)
                else:
                    inList.append(rInter)
            else:
                inList.append(rTerm)
        

        #finalTerms.append(inList)
        rt2 = rhs[1]
        if rt2 == outerListOperand:
            for rTerm in inList:
                finalTerms.append(rTerm) 
            
            #finalTerms.append(outerListOperand)
        else:
            finalTerms.append([inList])'''

    else:
        #finalTerms.append(outerListOperand)
        finalTerms.append(rhs)


    return finalTerms #[lbOpen + outerListOperand + rbOpen]

def appendAndOr(termOfOr , listOfAndTerms):
    print 'appendAndOr on....', termOfOr, listOfAndTerms

    listOfAndTerms = filter(lambda a: a != '&', listOfAndTerms)

    #listOfAndTerms.remove('&')
    CNFList = []
    for term in listOfAndTerms:
        l1 = []
        l1.append(termOfOr)
        l1.append('|')
        l1.append(term)

        CNFList.append(l1)
        CNFList.append('&')
    print '=========after distributing OR over AND:', CNFList
    CNFList= CNFList[:-1]
    
    return CNFList




#finalOp = []


def doCNFStep3(parsed_op_CNF2):
    print 'doCNFStep3 on......', parsed_op_CNF2
    #global finalOp
    finalOp =[]
    prevTerm = None
    #possible:  
    #        A | (B...) 
    #==OR== (A...) | B 
    #==OR== (A..) | (B..) 
    t1 = parsed_op_CNF2[0]
    t2 = parsed_op_CNF2[1]
    t3 = parsed_op_CNF2[2]

    #A|B or A&B
    if type(t1) is not list and type(t3) is not list:
        print 't1 is not a list and t3 is also not a list'
        #finalOp.append([t1])
        #finalOp.append(t2)
        #finalOp.append([t3])

        return parsed_op_CNF2
    #only second one is a list
    #A| (B..)   =====   if B has an & then   A|B  & A|...
    elif type(t1) is not list:
        print 't1 is not a list and t3 is  a list'
        ret = doCNFStep3(t3)
        print 'now trying to append t1 to terms of t3 list ', ret, t1

        if  t2 == '|' :
            if '&' in ret: #conditino when we need to distribute | over the inner and returned
                finalOp = appendAndOr(t1,ret)
            else: #no & in the  list?????
                print 'No & in the  list ......' , ret
                finalOp.append(t1 )
                finalOp.append(t2)
                finalOp.append( ret)
                print 'finalOP!!', finalOp
        else: #no | in the final list
            print 'No | in the final list, so no distribution needed?' , t1 , 'ret....', ret
            finalOp.append(t1 )
            finalOp.append(t2)
            finalOp.append( ret)
            print 'finalOP!!', finalOp
 
    elif type(t3) is not list:
        print 't1  is a list and t3 is not a list'
        ret = doCNFStep3(t1)
        print 'now trying to append t3 to terms of t1 list ', ret, t3

        if  t2 == '|' :
            if '&' in ret: #conditino when we need to distribute | over the inner and returned
                finalOp = appendAndOr(t3,ret)
            else: #no & in the  list?????
                print 'No & in the  list ......' , ret
                finalOp.append(ret)
                finalOp.append(t2)
                finalOp.append(t3 )
                print 'finalOP!!' ,ret, 't2:', t2 , t3
        else: #no | in the final list
            print 'No | in the final list, so no distribution needed?' , t1 , 'ret....', ret
            finalOp.append( ret)
            finalOp.append(t2)
            finalOp.append(t1 )
            print 'finalOP!!' , ret, 't2:', t2 , t3
    
    else:
        print 't1 is : ', t1
        ret1 = doCNFStep3(t1)
        ret3 = doCNFStep3(t3)
        print 'BOTH LISTS' , t2
        print  ret1
        print  ret3
        #return 
        
        if t2 == '|':
            if '&' not in ret1 and '&' not in ret2:
                finalOp.append(ret1)
                finalOp.append(t2)
                finalOp.append(ret3)

            elif '&' not in ret1: # but & in ret2
                finalOp = appendAndOr( ret1 ,ret3)
            elif '&' not in ret3:
                finalOp = appendAndOr(ret3, ret1)

            else:# both listst have an & term and are seperated by an | in the final list, take each of ret1 terms and append to ret 2 terms
                for ret3Term in ret3:

                    interOp = appendAndOr(ret3Term, ret1)
                    finalOp.append(interOp)
                    finalOp.append('&')
                finalOp= finalOp[:-1]

        #else:



    print 'returning .....', finalOp

    return finalOp

#orig_sentence = "((( ~( A(x) & N(o) )|(B(y)=>V(c)))&D(v) )=> (C(v)=>F(v)))"

#orig_sentence = "(((A(x)|B(y))&D(v) )=>C(v))"
#orig_sentence =  "((A(x)=>B(y))=>C(v))"
#parsed_input =  parse("((A(x)=>B(y))=>C(v))") # 


#orig_sentence = "(( (A(x) => N(o)) & D(v))=> C(x))"

#orig_sentence = "( ( (A(x)=>B(i)) & D(v)) => C(x))"
orig_sentence = "( ( (A(x)=>B(i)) & D(v)) => (C(x) => (~R(v)) ) )"
#orig_sentence = " ( ((A(x)=>B(i)) & D(v)) => ((C(x,y) &V(n))=> R(v)))"
#orig_sentence = " ( ( (~A(x))  & ((~B(y)) & F(i)) )| C(z)) "


#orig_sentence = " ( ( (~A(x))  & (~B(y))) | C(z)) "



inputData = [ "A(x)& C(n)" 
            "( ( (A(x)=>B(i)) & D(v)) => (C(x) => (~R(v)) ) )"
            
            ]

for sent in inputData:
    stanSent = Standardize(sent)

    parsed_input =  parse(orig_sentence) # 

    #print 'first level parsed:', parsed_input

    newP1 = doCNFStep1(parsed_input)

    #print 'orig sentence:', parsed_input
    #print '\n CNF op1.....',newP1

    #print '\n \n'

    newP2 = doCNFStep2(newP1)
    print 'unparsed original sentence: ', orig_sentence
    print 'parsed sentence:', parsed_input
    print 'CNF1 op: ',newP1
    print 'CNF 2 op: ',newP2




    #op = bracketOpen(newP2)
    #print '\n\nAfter bracket opening....', op
    op = newP2

    print '\n\n'
    newP3 = doCNFStep3(op)
    print 'orig sentence:', parsed_input
    print 'CNF1 op: ',newP1
    print 'CNF 2 op: ',newP2
    print 'CNF 3 op: ',newP3

    bRem = bracketOpen(newP3)
    print '\n\n after bopen: ', bRem

    #print parse("((Dog(y) & Owns(x, y)) => Animallover(x))") #[[['Dog', ['y']], '&', ['Owns', ['x', 'y']]], '=>', ['Animallover', ['x']]]
    #print parse("(Kills(Jack, Tuna) | Kills(Curiosity, Tuna))") #[['Kills', ['Jack', 'Tuna']], '|', ['Kills', ['Curiosity', 'Tuna']]]
    #print parse("(~(Kills(Curiosity, Tuna)))")#['~', ['Kills', ['Curiosity', 'Tuna']]]























print "--------------------------TODO: CHANGE CODE--------------------"

