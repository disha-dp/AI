import ply
import copy

index = 0
theta = {}


queries = []
inputSentences = []
#file read
def read_data():
    global inputSentences, queries
    file_data = open("input.txt")
    print file_data
    NQ = int(file_data.readline().strip())
    
    for _ in range(NQ):
        queries.append(file_data.readline().strip())

    NS = int(file_data.readline().strip())
    for _ in range(NS):
        inputSentences.append(file_data.readline())


try:
    from pyparsing import (alphanums, alphas, delimitedList, Forward,nums,Optional,Group, Literal, opAssoc, operatorPrecedence, ParseSyntaxException, ParseException,
            Word)
except ImportError:
    pass 
print "--------------------------TODO: CHANGE CODE--------------------"



def actCombineWord(  word):
    global index
    word = list(word)

    idxOpen = (word).index('(')
    part1 = word[:idxOpen+1]
    part2 = ""
    for x in range(idxOpen+1 , len(word) -1):
        part2 +=  word[x] 
        if word[x][0].islower():
            part2 += str(index)
        part2 +=','
    part2 = part2[:-1]
    part3 = ')'

    return [''.join(x for x in part1) +part2+ part3]    

def parse(text ):
    global index
    index+=1

    left_parenthesis, right_parenthesis =  "(",")"
    
    implies = Literal("=>")
    or_ = Literal("|")
    and_ = Literal("&")
    not_ = Literal("~")
    symbol = (Optional(not_)+ Word(alphas) + left_parenthesis + delimitedList(Word(alphas)) + right_parenthesis).setParseAction(actCombineWord) #, alphanums) 
    term = Forward()
    
    term << (Group(symbol + Group(left_parenthesis +
                   delimitedList(term) + right_parenthesis)) | symbol)


    #term << (Group(symbol + Group(left_parenthesis + symbol + right_parenthesis)) |symbol )

    formula = Forward()
    operand = term

    formula << operatorPrecedence(operand, [
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
    #print  'outerListOperand :  ',outerListOperand
    
    #if type(lhs) is not list and type(rhs) is not list :

    if type(lhs) is list:
        lInter = bracketOpen(lhs)
        #print 'converted lhs using bOpen....', lInter
        innerLHSOper = lhs[1]
        if innerLHSOper == outerListOperand:
            for term in lInter:
                finalTerms.append( term )
        else:
            finalTerms.append(lInter)
    else:
        finalTerms.append(lhs)

    finalTerms.append(outerListOperand)

    if type(rhs) is list:
        rInter = bracketOpen(rhs)
        #print 'converted rhs using bOpen....', rInter
        innerRHSOper = rInter[1]
        if innerRHSOper == outerListOperand:

            for term in rInter:
                finalTerms.append( term )
        else:
            finalTerms.append(rInter)
        #finalTerms.append( rInter )
        
   
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
    #print '=========after distributing OR over AND:', CNFList
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
        #print 't1 is not a list and t3 is also not a list'

        return parsed_op_CNF2
    #only second one is a list
    #A| (B..)   =====   if B has an & then   A|B  & A|...
    elif type(t1) is not list:
        #print 't1 is not a list and t3 is  a list'
        ret = doCNFStep3(t3)
        #print 'now trying to append t1 to terms of t3 list ', ret, t1

        if  t2 == '|' :
            if '&' in ret: #conditino when we need to distribute | over the inner and returned
                finalOp = appendAndOr(t1,ret)
            else: #no & in the  list?????
                #print 'No & in the  list ......' , ret
                finalOp.append(t1 )
                finalOp.append(t2)
                finalOp.append( ret)
                #print 'finalOP!!', finalOp
        else: #no | in the final list
            #print 'No | in the final list, so no distribution needed?' , t1 , 'ret....', ret
            finalOp.append(t1 )
            finalOp.append(t2)
            finalOp.append( ret)
            #print 'finalOP!!', finalOp
 
    elif type(t3) is not list:
        #print 't1  is a list and t3 is not a list'
        ret = doCNFStep3(t1)
        #print 'now trying to append t3 to terms of t1 list ', ret, t3

        if  t2 == '|' :
            if '&' in ret: #conditino when we need to distribute | over the inner and returned
                finalOp = appendAndOr(t3,ret)
            else: #no & in the  list?????
                #print 'No & in the  list ......' , ret
                finalOp.append(ret)
                finalOp.append(t2)
                finalOp.append(t3 )
                #print 'finalOP!!' ,ret, 't2:', t2 , t3
        else: #no | in the final list
            #print 'No | in the final list, so no distribution needed?' , t1 , 'ret....', ret
            finalOp.append( ret)
            finalOp.append(t2)
            finalOp.append(t1 )
            #print 'finalOP!!' , ret, 't2:', t2 , t3
    
    else:
        #print 't1 is : ', t1
        ret1 = doCNFStep3(t1)
        ret3 = doCNFStep3(t3)
        #print 'BOTH LISTS' , t2
        #print  ret1
        #print  ret3
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


def applyNegation1(query):
    return ['~' + ''.join(x for x in  query)]

def negPred(pred):
    if pred[0]== "~":
        return pred[1:]
    #return '~' + pred
    return '~' + ''.join(x for x in  pred)


def compound(term):
    if '(' not in term :
        return False
    return True

def unify_var(var,x,theta):#x is diff 
    all_keys = theta.keys()
    if  var in all_keys:
        return unify(theta[var],x,theta)

    elif x in all_keys:
        return unify(var,theta[x],theta)
    else:
        theta[var]= x
        print 'theta from unify var:',theta
        return theta



def unify(x,y,theta):
    #x = variable / constant / list / compound expression
    #y = variable / constant / list / compound expression 
    print 'unify..... x:',x, 'y:',y ,theta
    if '(' not in x and ',' in x and '(' not in y and  ',' in y:
        x = x.split(',')
        y = y.split(',')
    print 'unify..... x:',x, 'y:',y
    #print theta
    if theta ==None:
        return None

    elif x == y:
        print 'equal x and y!!!'
        return theta

    elif type(x) is str and  x.islower():
        print 'x[0] is lower!' ,x,y,'Call unify Var'
        return unify_var(x, y, theta)
 
    elif type(y) is str and y.islower():
        print 'y[0] is lower!' ,x,y,'Call unify Var'

        return unify_var(y,x, theta)

    elif compound(x) and compound(y): #Father(x,y) 
        opBracX = x.index('(')
        opBracY = y.index('(')
        return unify(x[opBracX+1:-1], y[opBracY+1:-1],unify(x[:opBracX] ,y[:opBracY],theta))

    elif type(x) is list and type(y) is list:#get param by param
        print 'WE HAVE LIST OF ARGS!!!', x , y  
        return unify(x[1:] ,y[1:],unify(x[0],y[0],theta))

    else :
        return None 



def putValForVar(clause, k, val):
    return clause[0].replace(k,val)


def substitute_theta(resolved_clause , theta):
    
    sent = copy.deepcopy(resolved_clause)

    print 'substitute_theta ....', sent , theta
    for key in theta:
        #print 'k: ', key , 'vals: ', theta[key]
        vals = theta[key]
        k= key
        len_clause = len(sent)
        for idx in range(len_clause):
            sent[idx] = putValForVar(sent, k,vals )
    return sent


def standardize(clause):
    global index
    for pred in clause:
        idxOpenBrac = pred.index('(')
        for pred in clause:
            part1 =  pred[:idxOpenBrac]
            allLiterals = pred[idxOpenBrac+1 : -1].split(',')
            part2 = ""
            for x in allLiterals:
                part2 +=  x 
                if x[0].islower():
                    part2 += str(index)
                part2 +=','
            part2 = part2[:-1]

    index+=1



def union(a, b):
    return list(set(a) | set(b))


def resolve(c1,c2):
    c1copy = copy.deepcopy(c1)
    c2copy = copy.deepcopy(c2)
    global theta
    resolved_clauses =[]#//list of resolved claueses. more than one literal can resolve with another in the two senteces. 
                           #??????//Create one for each. There should be an unification with a constant existing. Else fail the unification.
    
    #//check if p1 and p2 are negation of eachother. Basically check for conditions for resolution.
    #print 'resolve: c1', c1copy
    #print 'resolve with c2', c2copy
    for  p1,p2 in zip(c1copy,c2copy):#do 
        #print 'try unifying: p1 :',p1, 'and p2: ',p2
        p1m = p1
        p2m = p2
        if p1[0]=='~':
            p1m=p1[1:]

        if p2[0]=='~':
            p2m=p2[1:]
        print 'p1!! ', p1 , 'p2: ', p2
        theta = unify(p1m,p2m,theta) 
        #print c1copy, c2copy  

        print theta
        if theta:
            c1copy.remove(p1)
            c2copy.remove(p2)
            resolved_clause = []
            preds = union(c1copy,c2copy)
            for p in preds:
                resolved_clause.append(p)  #union({prediactes(c1) | p1},{predicates(c2) | p2}) #//add all predicates except p1 and p2
            print 'resolved clause is: ', resolved_clause
            if len(resolved_clause)==0: 
                print 'empty list!!!!!'
                continue
            substitutedStr = substitute_theta(resolved_clause ,theta )
            print 'substituted clause....' , resolved_clause ,substitutedStr
            standardize(substitutedStr) #//This is important because, a new clause is created and it should have new variables than the ones in kb.
            
            resolved_clauses.append(resolved_clause)
        else:
            print'no resolution was possinle '
    return resolved_clauses



def resolution( KB, query ):
    global mapPC
    resolvents = [] #use a priority queue that sorts the claueses based on number of literals.
    negated_query = applyNegation1(query)
    resolvents.append(negated_query)
    resolvents.sort(key=len)
    num_iter = 0
    max_iter = 10000
    
    while len(resolvents) > 0  and num_iter < max_iter :
        print 'resolvents now: ',resolvents
        num_iter = num_iter + 1
        c1 = resolvents[0]
        resolvents = resolvents[1:]

        possible_clauses = []
        
        for predicate in c1 : #creates all possible subsets of claueses that c1 can unify with
            print 'predicate in clause: ' ,predicate
            negatedPred = negPred(predicate)
            actualPred = negatedPred[:negatedPred.index('(')]
            print 'negated predicate to check in KB: ', negatedPred[:negatedPred.index('(')] 
            if  actualPred in mapPC.keys()  :#then

                possible_clauses.append( mapPC[actualPred] )
        
        #if there exist no possible clause continue to next iterations
        if len(possible_clauses) == 0 :
            print 'NO POSSIBLE CLAUSES!!!'
            continue

        newly_resolved =[]  #<- {}
        for  c2 in possible_clauses[0] :
            if c1 != c2 :    #then //want to resolve different claueses only, to avoid resolving A(X) | ~A(X) with itself
                resolved_clauses = resolve(c1,c2)   #//method that resolves using the resolution rule. written as a separate algorithm
                for  resolved_clause in resolved_clauses:# do
                    if len(resolved_clause) ==0:    #    is empty :
                        return True #//contradiction arised

                    if resolved_clause not in KB:# then  //add new clauses to the list of clauses
                        newly_resolved.append(resolved_clause)
                        KB.append(resolved_clause)
                        resolvents.append(resolved_clause)
                        resolvents.sort(key=len)
        appendMapPC( newly_resolved )   
    return False # //comes here when there is nothing more to resolve



mapPC ={}
def appendMapPC(KB):
    print 'append MapPC on', KB
    global mapPC

    for sent in KB:
        for term in sent:
            if type(term) is list:
                appendMapPC([term])
            else:
                #print 'term in appendMapPC: ',type(term)    
                openBracketIndex = term.index('(')
                startIndex =0

                pred = term[startIndex:openBracketIndex]
                literals = term[openBracketIndex:len(term)-1].split(',')

                if pred not in mapPC:
                    mapPC[pred] = []

                if sent not in mapPC[pred]:
                    mapPC[pred].append(sent)
    #print mapPC

inp_file = open("input.txt")

#inputData = [ "(A(x,y)& C(Nerd))" ,
#              "(A(x,y) | D(x))" ,

#            "(  ( A(x)=> (B(i) & D(v))) => (C(x) => (~R(v)) ) )"
#            ]

query = ["A(Bob, Pat)"]
KB = []



read_data()

print inputSentences
for i,sent in enumerate(inputSentences):
    print 'sent in consideration------',sent
    parsed_input =  parse(sent) # 

    #print 'first level parsed:', parsed_input
    #stanSent = Standardize(parsed_input)

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
    
    andList = []
    orList = []
    for t in bRem:
        print 'and list looks like....',andList
        if t == '&':
            if type(andList[0]) is list:
                KB.append( filter(lambda a: a != '|', andList[0]))
            else:
                KB.append(andList)

            andList =[]
        elif t == '|':
            continue
        else:
            andList.append(t)
            orList.append(t)

    if type(andList[0]) is list:
        KB.append( filter(lambda a: a != '|', andList[0]))
        #KB.append(andList[0])
    else:
        KB.append(andList)

    #print 'KBNOw:', KB
#print 'KB: ', KB
mapper  = {}

appendMapPC(KB)
print '\n\n\n\n\n\n\n========================Starting resolution========================\n\n\n\n\n\n'
resolution(KB, query)

print "--------------------------TODO: CHANGE CODE--------------------"

