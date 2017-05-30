fn = "/Users/schoem/Languages/R code/Machine:StatisticalLearning/DecisionTree/PlayTennis.csv"
df=read.csv(fn,header=TRUE,stringsAsFactors = FALSE)
df[,1] = ifelse(df[,1] == "No",0,1)

#

entropy = function(cnts){
  res = 0
  if (!(0 %in% cnts)) {
    tot = sum(cnts)
    res = -(cnts[1]/tot*log2(cnts[1]/tot) + cnts[2]/tot*log2(cnts[2]/tot))
  }
  return(res)
}

get.counts = function(df,label,attrib,val){
  targ = df[,attrib]
  indicies = which(targ == val)
  s = sum(df[indicies,label])
  return(c(s,length(indicies)-s))
}

info.gain = function(S,n,cnts,ents){ #S = root entropy, n= number in root
  tot_attribute = 0
  for (i in 1:length(ents)){
    tot_attribute = tot_attribute + sum(cnts[[i]]/n)*ents[i]
  }
  return(S - tot_attribute)
}

get.switch.node = function(ss,decision,nodes,num_success,num_fail) {
  root_entropy = entropy(c(num_success,num_fail))
  num_items = num_success + num_fail
  info_gain = numeric(length(nodes))
  for (i in 1:length(nodes)){
    node = nodes[[i]]
    cnts=lapply(node$edges,function(v){get.counts(ss,decision,node$attrib,v)})
    ents = sapply(cnts,function(c){entropy(c)})
    info_gain[i] = info.gain(root_entropy,num_items,cnts,ents)
  }
  return(nodes[[which.max(info_gain)]])
}

make.node = function(attribute,vals,ss,targ) {
  node = list(attrib=attribute, num_edges=length(vals),edges = vals,
       children=vector("list",length(vals))
       #cnts=lapply(vals,function(v){get.counts(ss,targ,attribute,v)})
       )
  class(node) = "Node"
  node
}
get.node.attributes = function(nodes){
    sapply(nodes,function(obj){obj$attrib})}
get.node = function(nodes,attrib) {
    nodes(which(sapply(nodes,function(obj) obj$attrib == attrib)))}
remove.node = function(nodes,node,attrib) {
    nodes[-which(get.node.attributes(nodes) == node$attrib)]}

make.leaf = function(num_success,num_fail){
  obj = c(success=num_success,fail=num_fail)
  class(obj) = "Leaf"
  obj
}

make.nodes = function(df,atts,targ) {
  vals = sapply(atts,function(name) unique(df[,name]))
  lapply(1:length(atts),function(i) make.node(atts[i],vals[[i]],df,targ))
}

make.root = function(ss,decision,atts) {
  num_success = sum(ss[,decision])
  num_fail = nrow(ss) - num_success
  return(get.switch.node(ss,decision,atts,num_success,num_fail))
}


process.node = function(root,decision,nodes,df) {
  print(c("node",root$attrib,root$edges,get.node.attributes(nodes)))
  for (val in root$edges) {
    print(c(root$attrib,val))
    ss = subset(df,df[,root$attrib] == val,
                select=c(decision,get.node.attributes(nodes)))
    num_success = sum(ss[,decision])
    num_fail = nrow(ss) - num_success
    print(c(num_success,num_fail))
    if ((num_success == 0) | (num_fail == 0)) {
      leaf = make.leaf(num_success,num_fail)
      root$children[[which(root$edges == val)]] = leaf
      leaves[[length(leaves)+1]] <<- leaf
    } else {
      switch_on = get.switch.node(ss,decision,nodes,num_success,num_fail)
      nodes = nodes[-which(get.node.attributes(nodes) == switch_on$attrib)]
      temp <<-process.node(switch_on,decision,nodes,ss)
      root$children[[which(root$edges == val)]] = temp
    }  
  }
  return(root)
}

#
decision = colnames(df)[1]
leaves = list()
nodes = make.nodes(df,colnames(df)[-1],decision)
root = make.root(df,decision,nodes)
nodes = remove.node(nodes,root) 
tree = process.node(root,decision,nodes,df)


      
  

