#' @title Summary table from recurrence data in a data frame
#' 
#' @description
#' SummaryTable automatically creates summary tables in latex format.
#' The function calculates the frequency and percent(optional) of
#' each level of each categorical row variable(rowvars) for each level
#' of each column variable(colvars).
#' 
#' @details
#' The core function of the table builder consists of repeated calls
#' to a \code{tapply} statment:
#' \code{table <- tapply(data$count, list(rowlist[[i]],data[ ,colvar]),sum)}
#' 
#' where data$count is a function defined variable with a \code{1} in each
#' row for tabulating frequencies. \code{rowlist[[i]]} represents each row variable
#' and \code{data[ ,colvar]} is the column variable.
#' Currently the function does not support \code{NA} values in the column variable.
#' 
#' 
#' @usage 
#' SummaryTable(data,rowvars,colvar,row.names=NULL,cont.vars=NULL,percent=TRUE,output=c("latex","matrix"), reorder.columns=NULL,
#' cap=NULL,shortcap=NULL,lab=NULL,rnd.val=1,var.totals=FALSE,missing=TRUE,outfile="~/table1output.rtf",rtf.font.size=11,
#' col.widths=NULL, col.justify=('C'), header.col.justify=('C'),
#' rtf.row.names=FALSE, NA.string="-", space.before=NULL, space.after=NULL,
#' rmark.split=Inf)
#' 
#' 
#' @param data matrix or data frame with the row and column variables
#' @param rowvars a character vector of quoted variable names or a numeric vector
#' of categorical variables
#' @param colvar a quoted variable name or a number identifying the column variable
#' in the data
#' @param row.names an optional character vector of row names if you want to change
#' the displayed name for row variables. \code{len(row.names)} must equal \code{len(rowvars)}
#' @param cont.vars a character vector of variable names, or a number vector of column indices
#' @param percent logical indicating whether to print percentages for each row
#' @param output a vector of outputs including one or more of \code{latex,matrix,rtf}
#' @param reorder.columns optional character string specifying the column level 
#' to appear first in the table
#' @param cap character string with the desired table caption
#' @param shortcap Character string with desired short caption to use in list of tables.
#' @param lab character string with table label
#' @param rnd.val number of digits to round to
#' @param var.totals logical indicating whether to print totals for each row variable
#' @param missing logical indicating whether to create a row within each
#' row variable for NA values
#' @param outfile if \code{output='rtf'}, a file path to save the output
#' @param rtf.font.size if \code{output='rtf'}, the size of the font in the rtf table
#' @param col.widths if \code{output='rtf'}, a vector of column widths
#' @param col.justify if \code{output='rtf'}, a vector of column alignments
#' @param header.col.justify if \code{output='rtf'}, a vector of alingments for column headers
#' @param rtf.row.names if \code{output='rtf'}, logicial indicating whether to print row names
#' @param NA.string if \code{output='rtf'}, string used to replace NA
#' @param space.before if \code{output='rtf'}, see package rtf for details
#' @param space.after if \code{output='rtf'}, see package rtf for details
#' 
#' 
#' @examples
#' \donttest{
#' data(BDSSRecurrenceExample)
#' fr <- BDSSRecurrenceExample
#' SummaryTable(data=fr,rowvars=c("treated","trt2","single"),colvar="Recur",cont.vars=c("Age","meas"))
#' }
#' 
#' @export
#' @name SummaryTable
#' @author Dominic LaRoche


SummaryTable<-function(data,rowvars,colvar,row.names=NULL,cont.vars=NULL,percent=TRUE,output=c("latex","matrix"), reorder.columns=NULL,
                 cap=NULL, shortcap=NULL ,lab=NULL, rnd.val=1, var.totals=FALSE, missing=TRUE, outfile="~/table1output.rtf", rtf.font.size=11,
                 col.widths=NULL, col.justify=('C'), header.col.justify=('C'),
                 rtf.row.names=FALSE, NA.string="-", space.before=NULL, space.after=NULL,
                 rmark.split=Inf){
    
  
  #convert variable names to indicies
  if(is.character(rowvars[1])){
    for (i in 1:length(rowvars)){
      rowvars[i]<-which(names(data)==rowvars[i])
    }
    rowvars<-as.numeric(as.character(rowvars))
  }
  if(is.character(colvar)){
    colvar<-which(names(data)==colvar)
  }
  if(is.character(cont.vars[1])){
    for (i in 1:length(cont.vars)){
      cont.vars[i]<-which(names(data)==cont.vars[i])
    }
    cont.vars<-as.numeric(as.character(cont.vars))
  }
  
  if(any(is.na(unique(data[,colvar])))){
    stop("The column variable chosen has NA values. This will affect frequency tabulation.\n  This function is not designed for use with NA values in the column variable.")
  }
  if(!is.factor(data[,colvar])) stop("The column variable is not a factor.")
  if(!is.null(reorder.columns)){
    data[,colvar]<-relevel(data[,colvar],reorder.columns)
  }
  meansd<-function(x){ #function to compute the mean and standard error of a continuous variable
    m<-round(mean(na.omit(x)),rnd.val)
    s<-round(sd(na.omit(x)),rnd.val)
    return(paste0(m," ","(",s,")"))
  }
  #This is a subfunction which adss percent columns to the final table
  add.percent<-function(tbl){
    ntbl<-matrix(tbl[,1],dim(tbl)[1],1)#initialize new matrix with first line of the input table
    colnames(ntbl)<-colnames(tbl)[1]
    for(i in 1:dim(tbl)[2]){#loop through each column in the table and add percent column
      sumcol<-sum(as.numeric(tbl[,i]))
      p<-paste0(round(100*(as.numeric(tbl[,i])/sumcol),rnd.val),"%")
      if(i==dim(tbl)[2]){
        ntbl<-cbind(ntbl,p)
        j<-dim(ntbl)[2]
        colnames(ntbl)[j]<-paste(colnames(tbl)[i],"%",sep=" ")
        
      }else{
        ntbl<-cbind(ntbl,p,tbl[,i+1])
        j<-dim(ntbl)[2]-1
        k<-j+1
        colnames(ntbl)[j:k]<-c(paste(colnames(tbl)[i],"%",sep=" "),colnames(tbl)[i+1])
      }
    }
    if(var.totals==TRUE){#if variable totals are desired within the table make a total line for each rvariable
      Total<-numeric(length(dim(ntbl)[2]))
      for(i in 1:(dim(ntbl)[2]-1)){#add bottom total row
        main<-seq(1,dim(ntbl)[2],2)
        perc<-seq(2,dim(ntbl)[2],2)
        if(any(main==i)){#sum the count values
          Total[i]<-sum(as.numeric(ntbl[,i]))
        }
        if(any(perc==i)){#get the percent of the total for the variable
          Total[i]<-paste0(round(100*(as.numeric(Total[i-1])/sum(as.numeric(ntbl[,dim(ntbl)[2]-1]))),rnd.val),"%")
          
        }
      }
      
      #make the last total percent the percent of the number of rows in the database for error checking
      Total[dim(ntbl)[2]]<-paste0(round(100*(as.numeric(Total[dim(ntbl)[2]-1])/dim(data)[1])),"%")
      ftbl<-rbind(ntbl,Total)
      return(ftbl)
    }else{
      return(ntbl)
    }    
  }
  
  #Start main function
  data$count<-rep(1,length(dim(data)[1]))#add a simple variable for counting rows
  if(is.null(row.names)){
    rowvarnames<-names(data)[rowvars]#use variable names in the dataframe if none supplied
  }else{
    rowvarnames<-row.names
  }
  
  colvarname<-names(data)[colvar]
  
  rowlist<-vector(mode="list",length(rowvars))#make the list of row objects
  for (i in 1:length(rowvars)){
    if(missing==TRUE){
      rowlist[[i]]<-factor(data[,rowvars[i]],exclude=NULL)#capture NA's with exclude=NULL
    }else{
      rowlist[[i]]<-factor(data[,rowvars[i]])
    }
    names(rowlist)[i]<-rowvarnames[i]
  }
  
  row.tables<-vector(mode="list",length(rowvars))
  #This is the heart of the funtion which tabulates all the row variables for all the levels of the column variable
  for (i in 1:length(rowvars)){#loop through each row var and make a table.  
    table<-tapply(data$count,list(rowlist[[i]],data[,colvar]),sum)#The tabulation step
    
    if(any(is.na(table))){#check for NA's in table and change to 0's 
      #(this occurs when there is no combinaton of the row value and col value in the data)
      table[is.na(table)]<-0
    }
    Total<-apply(table,1,sum)#make total row
    table<-cbind(table,Total) 
    if(missing==TRUE){
      rownames(table)[is.na(rownames(table))]<-"Missing"#convert NA row label to string
    }
    
    if(percent==TRUE){#check if percents wanted
      row.tables[[i]]<-add.percent(tbl=table)
    }else{#add column names and botom row if not using percents
      if(var.totals==TRUE){#if variable totals are desired within the table make a total line for each rvariable
        Total<-numeric(length(dim(table)[2]))#shell for bottom row
        for(j in 1:(dim(table)[2])){#add bottom total row
          Total[j]<-sum(table[,j])
        }
        ftbl<-rbind(table,Total)
        row.tables[[i]]<-ftbl
        
      }else{
        row.tables[[i]]<-table
      }
    }
  }
  names(row.tables)<-rowvarnames
  #correct row names for variables with only 1 level
  for(i in 1:length(row.tables)){
    if(dim(row.tables[[i]])[1]==1){
      rownames(row.tables[[i]])<-levels(rowlist[[i]])
    }
  }
  submat<-vector(mode="list",length(row.tables))
  for (i in 1:length(row.tables)){#add variable names and levels
    rl<-nrow(row.tables[[i]])#the number of levels of each variable
    cl<-length(unique(data[,colvar]))
    mat<-cbind(matrix(c(names(row.tables)[i],rep("",rl-1),rownames(row.tables[[i]])),rl,2),row.tables[[i]])
    submat[[i]]<-rbind(mat,rep("",dim(mat)[2]))
  }
  
  if(percent==TRUE){
    header<-c("Variable","Categories",rep(c("N","%"),cl+1))
  }else{
    header<-c("Variable","Categories",rep(c("N"),cl+1))
  }
  out.table<-do.call(rbind,submat)
  out.table<-rbind(header,out.table)
  
  MainTotal<-character(dim(out.table)[2])#get column totals for each column variable
  lev.list<-levels(data[,colvar])
  tot<-numeric(length(levels(data[,colvar])))
  MainTotal<-c("Total","")
  if(percent==FALSE){
    for(i in 3:(dim(out.table)[2]-1)){
      tot[i-2]<-sum(data[,colvar]==lev.list[which(lev.list==colnames(out.table)[i])])
      MainTotal[i]<-tot[i-2]
    }
    MainTotal[dim(out.table)[2]]<-dim(data)[1]
    if(sum(tot)!=dim(data)[1]) warning("The total values of the columns is not equal to the data dimension:\n  There are likely NA values in the column variable")
  }else{
    l<-2
    for(k in 3:(dim(out.table)[2]-2)){
      main<-seq(1,dim(out.table)[2],2)
      perc<-seq(2,dim(out.table)[2],2)
      if(any(main==k)){#sum the count values
        MainTotal[k]<-sum(data[,colvar]==lev.list[which(lev.list==colnames(out.table)[k])])
        tot[k-l]<-sum(data[,colvar]==lev.list[which(lev.list==colnames(out.table)[k])])
        l<-l+1
      }
      if(any(perc==k)){#get the percent of the total for the variable
        
        MainTotal[k]<-paste0(round(100*(as.numeric(MainTotal[k-1])/dim(data)[1]),rnd.val),"%")
        
      }
      
    }
    MainTotal[dim(out.table)[2]-1]<-dim(data)[1]
    if(sum(tot)!=dim(data)[1]) warning("The total values of the columns is not equal to the data dimension:\n  There are likely NA values in the column variable")
    MainTotal[dim(out.table)[2]]<-NA
  }
  
  #make rows for the continuous variables
  if(!is.null(cont.vars)){#check to see if continuous variables have been passed to the function
    cont.row<-vector(mode="list",length(cont.vars))
    cont.row.latex<-vector(mode="list",length(cont.vars))
    for(i in 1:length(cont.vars)){#make a row for each continuous variable
      entry<-paste(names(data)[cont.vars[i]]," ",sep=" & ")#get variable name and add empty character to match format
      plain<-c(names(data)[cont.vars[i]],"")
      for(p in 1:length(unique(data[,colvar]))){#get mean and sd for each level of the ith continuous variable
        entry<-paste0(entry,"& \\multicolumn{2}{c}{",meansd(data[,cont.vars[i]][data[,colvar]==levels(data[,colvar])[p]]),"} ")
        if(percent==TRUE){
          plain<-c(plain,meansd(data[,cont.vars[i]][data[,colvar]==levels(data[,colvar])[p]]),"")
        }else{
          plain<-c(plain,meansd(data[,cont.vars[i]][data[,colvar]==levels(data[,colvar])[p]]))
        }
      }
      blank<-paste0(paste(rep(" & ",3+(2*length(levels(data[,colvar])))),collapse=""),"\\\\")#make a blank row for spacing
      cont.row.latex[[i]]<-c(paste0(entry,"& \\multicolumn{2}{c}{",meansd(data[,cont.vars[i]]),"}\\\\"),blank)
      if(percent==TRUE){
        cont.row[[i]]<-c(plain,meansd(data[,cont.vars[i]]),"")
      }else{
        cont.row[[i]]<-c(plain,meansd(data[,cont.vars[i]]))
      }
    }
  }
  
  if(any(output=="latex")){
    if(percent==TRUE){
      cnames<-levels(data[,colvar])
      col<-""
      for(i in 1:length(cnames)){#make major column headings
        col<-paste0(col,"& \\multicolumn{2}{c}{",cnames[i],"}")
      }
      infoline<-paste0("%Summary table produced by the SummaryTable function on: ", date())
      options(warn=(-1))
      table.body<-print(xtable::xtable(out.table,caption=cap,label=lab),include.rownames=FALSE,include.colnames=FALSE,hline.after=c(0,1),only.contents=TRUE,timestamp=NULL,print.results=FALSE)
      options(warn=(0))
      alignment<-paste(rep("l",dim(out.table)[2]),collapse="")
      tabular<-paste0("\\begin{tabular}{",alignment,"}")
      columns<-paste0("& ",col,"& \\multicolumn{2}{c}{Total}\\\\")
      if(!is.null(cont.vars)){
        cont.rows<-unlist(cont.row.latex)
      }
      TotalRow<-print(xtable::xtable(matrix(MainTotal,1,length(MainTotal))),include.rownames=FALSE,only.contents=TRUE,include.colnames=FALSE,timestamp=NULL,print.results=FALSE)
      if(!is.null(cap)& !is.null(shortcap)){
        caption<-paste0("\\caption[",shortcap,"]{",cap,"}")
      }else if(!is.null(cap)){
        caption<-paste0("\\caption{",cap,"}")
      }else{
        caption<-""
      }
      if(!is.null(lab)) label<-paste0("\\label{",lab,"}") else label<-""
      
      if(!is.null(cont.vars)){
        writeLines(c(infoline,"\\begin{table}[ht]","\\centering",tabular,columns,table.body,cont.rows,TotalRow,"\\end{tabular}",caption,label,"\\end{table}"))
      }else{
        writeLines(c(infoline,"\\begin{table}[ht]","\\centering",tabular,columns,table.body,TotalRow,"\\end{tabular}",caption,label,"\\end{table}"))
      }
      
    }else{
      colnames(out.table)[1:2]<-rep(" ",2)
      if(!is.null(cont.vars)){
        cont.row.latex<-cont.row
        for(i in 1:length(cont.row.latex)){#latexify the continuous rows
          cont.row.latex[[i]]<-rbind(cont.row.latex[[i]],rep("",length(cont.row[[i]])))
        }
        cont.rows<-do.call(rbind,cont.row.latex)
        options(warn=(-1))
        print(xtable::xtable(rbind(out.table,cont.rows,MainTotal),caption=cap,label=lab),include.rownames=FALSE,hline.after= c(0,1,nrow(out.table)+2,nrow(out.table)+3))
        options(warn=(0))
      }else{
        options(warn=(-1))
        print(xtable::xtable(rbind(out.table,MainTotal),caption=cap,label=lab),include.rownames=FALSE,hline.after= c(0,1,nrow(out.table),nrow(out.table)+1))
        options(warn=(0))
      }
    }
  }
  if(any(output=="matrix")){
    if(!is.null(cont.vars)){
      return(rbind(out.table,do.call(rbind,cont.row),MainTotal))
    }else{
      return(rbind(out.table,MainTotal))
    }
  }
  if(any(output=="rtf")){#need to refine the rtf output 
    
    doc<-rtf::RTF(file=outfile,font.size=rtf.font.size)#initialize rtf document
    if(!is.null(cont.vars)){
      out.matrix<-rbind(out.table,unlist(cont.row),rep("",length(dim(out.table)[2])),MainTotal)#make matrix to output
    }else{
      out.matrix<-rbind(out.table,rep("",length(dim(out.table)[2])),MainTotal)
    }
    rownames(out.matrix)<-as.character(1:dim(out.matrix)[1])#change row names to remove duplicates (we won't use thes anyways)
    colnames(out.matrix)[1:2]<-c(" "," ")
    addTable(doc,out.matrix,col.widths=col.widths, col.justify=col.justify, header.col.justify=header.col.justify,
             font.size=rtf.font.size, row.names=rtf.row.names, NA.string=NA.string, space.before=space.before, space.after=space.after)
    done(doc)#close rtf document
    
  }
  if(any(output=="rmarkdown")){#use pandoc to output r markdown
    require.anyway("pander",verbose=FALSE)
    if(!is.null(cont.vars)){
      out.markdown<-rbind(out.table,do.call(rbind,cont.row),MainTotal)#make matrix to output
    }else{
      out.markdown<-rbind(out.table,MainTotal)
    }
    rownames(out.markdown)<-1:dim(out.markdown)[1]#change row names to remove duplicates (we won't use these anyway)
    colnames(out.markdown)[1:2]<-c(" "," ")
    
    pandoc.table(out.markdown,style="rmarkdown",split.tables=rmark.split)
  }
  #Check that column names match results
  if(colnames(tapply(data$count,list(rowlist[[i]],data[,colvar]),sum))[1]!=levels(data[,colvar])[1]){
    warning("Something strange happened with your column variable.  Check the levels of the column variable and make sure the columns of the Table1 output are correct!" )
  }
}