#poisson Matrix
soccerutility.cs_poisson_df = function(tg,sup,df)
{
  UPPER_GOALS = 15
  eH = (tg + sup)/ (2)
  eA = (tg - sup)/ (2)
  
  goals_x = seq(0,UPPER_GOALS)  
  
  goals_h = dpois(x=goals_x,lambda=eH)
  goals_a = dpois(x=goals_x,lambda=eA)
  
  goals_mat = goals_h %o% goals_a
  
  #renormalise
  
  goals_mat = goals_mat / sum(goals_mat)
  
  #vanilla densities
  
  hwin = sum(goals_mat[lower.tri(goals_mat)])
  awin = sum(goals_mat[upper.tri(goals_mat)])
  draw_vanilla = sum(diag(goals_mat))
  
  #draw adjusted
  diag(goals_mat) = diag(goals_mat) * df
  
  draw_diff = df * draw_vanilla - draw_vanilla
  
  h_fac = (hwin - (hwin/(hwin + awin)) * draw_diff) / hwin
  a_fac = (awin - (awin/(hwin + awin)) * draw_diff) / awin
  
  goals_mat[lower.tri(goals_mat)] = goals_mat[lower.tri(goals_mat)] * h_fac
  goals_mat[upper.tri(goals_mat)] = goals_mat[upper.tri(goals_mat)] * a_fac
  
  return (goals_mat)
  
}

soccerutility.getmod = function(win_rat,m){
  mod = c(1.0,1.0)
  mod[1] = mod0(win_rat,m)
  mod[2] = mod0(1.0 - win_rat,m)
  return(mod)
}
prob_mult = function(p,m){  
  return (p*m/((1-p) + (p*m)))  
}
mod0 = function(win_rat,m){
  x = 1.0
  if(win_rat < 0.5)
  {
    if(win_rat < m)
    {
      x = 0.0
    }
    else
    {
      x = (win_rat - m) / (0.5 - m)
    }  
  }
  return(x)
}

#poisson Matrix
soccerutility.cs_102 = function(tg,sup,df,seg_par = c(1.0207810,1.0402487,1.0716502,1.0082614,0.35)){
  
  
  i = soccerutility.cs_binomial_drawfactor(tg,sup,1.0,TRIALS=94)
  hwin = sum(i[lower.tri(i)])
  awin = sum(i[upper.tri(i)])
  # 4 segments , variable on win % 
  
  mod_for_sup = soccerutility.getmod(hwin/(hwin + awin),seg_par[5])
  
  # seg 1 win by1 2:0 -> 1:0
  p = prob_mult(p = i[2,1],(seg_par[1]-1.0) * mod_for_sup[1] + 1.0)
  i[3,1] = i[3,1] + (i[2,1] - p)
  i[2.1] = p
  p = prob_mult(p = i[1,2],(seg_par[1]-1.0) * mod_for_sup[2] + 1.0)
  i[1,3] = i[1,3] + (i[1,2] - p)
  i[1,2] = p
  
  # seg 2 win by1. (possibly dynamic with sup%)    
  h_init = 0
  a_init = 0
  home = c(0,0,0,0)
  away = c(0,0,0,0)
  for(h in 3: 6)
  {
    h_init = h_init + i[h,h-1]
    a_init = a_init + i[h-1,h]
    home[h-2] = prob_mult(i[h,h-1],(seg_par[2]-1.0)* mod_for_sup[1] + 1.0)
    away[h-2] = prob_mult(i[h-1,h],(seg_par[2]-1.0)* mod_for_sup[2] + 1.0)    
    
    i[h,h-1] = home[h-2]
    i[h-1,h] = away[h-2]
  }
  
  hs = sum(home) - h_init
  as = sum(away) - a_init
  
  h_a = 0
  a_a = 0
  for(h in 4:11){
    for(j in 1:(h-2)){
      h_a = h_a + i[h,j]
      a_a = a_a + i[j,h]
    }    
  }
  for(h in 4:11)
  {
    for(j in 1: (h-2))
    {
      i[h,j] = i[h,j] - (hs * i[h,j]/h_a)
      i[j,h] = i[j,h] - (as * i[j,h]/a_a)
    }
    
  }
  
  # seg 3 : 0-0 inflated w.r.t 1-1
  p = prob_mult(i[1,1],seg_par[3])
  i[2,2] = i[2,2] + (i[1,1] - p)
  i[1,1] = p
  
  # seg 4 : 2-2 inflated w.r.t 3-3 ...
  p = prob_mult(i[3,3],seg_par[4])
  hd = i[4,4] + i[5,5] + i[6,6] + i[7,7]
  p_ = p - i[3,3]
  for(j in 4:7){
    i[j,j] = i[j,j] - (p_ * i[j,j]/hd)
  }
  i[3,3] = p
  
  #reapply the draw factor, nothing should have changed!!!!
  
  hwin = sum(i[lower.tri(i)])
  awin = sum(i[upper.tri(i)])
  draw_vanilla = sum(diag(i))
  
  #draw adjusted
  diag(i) = diag(i) * df
  
  draw_diff = df * draw_vanilla - draw_vanilla
  
  h_fac = (hwin - (hwin/(hwin + awin)) * draw_diff) / hwin
  a_fac = (awin - (awin/(hwin + awin)) * draw_diff) / awin
  
  i[lower.tri(i)] = i[lower.tri(i)] * h_fac
  i[upper.tri(i)] = i[upper.tri(i)] * a_fac
  
  return(i)
  
}

soccerutility.cs_poisson_df_fixed =  function(tg,sup,df){
  soccerutility.cs_poisson_df(tg,sup,1)
}

#Returns a cs matrix
soccerutility.cs_binomial_drawfactor = function(tg,sup,df, TRIALS){
  
  UPPER_GOALS = 15  
  pH = (tg + sup)/ (2 * TRIALS)
  pA = (tg - sup)/ (2 * TRIALS)
  
  goals_x = seq(0,UPPER_GOALS)  
  
  goals_h = dbinom(goals_x,TRIALS,pH)
  goals_a = dbinom(goals_x,TRIALS,pA)
  
  goals_mat = goals_h %o% goals_a
  
  #renormalise
  
  goals_mat = goals_mat / sum(goals_mat)
  
  #vanilla densities
  
  hwin = sum(goals_mat[lower.tri(goals_mat)])
  awin = sum(goals_mat[upper.tri(goals_mat)])
  draw_vanilla = sum(diag(goals_mat))
  
  #draw adjusted
  diag(goals_mat) = diag(goals_mat) * df
  
  draw_diff = df * draw_vanilla - draw_vanilla
  
  h_fac = (hwin - (hwin/(hwin + awin)) * draw_diff) / hwin
  a_fac = (awin - (awin/(hwin + awin)) * draw_diff) / awin
  
  goals_mat[lower.tri(goals_mat)] = goals_mat[lower.tri(goals_mat)] * h_fac
  goals_mat[upper.tri(goals_mat)] = goals_mat[upper.tri(goals_mat)] * a_fac
  
  return (goals_mat)
  
}

#returns an array of [home true,draw true, away true,hilo line, hilo under true, hilo over true]
soccerutility.output_mkts = function(cs_gn,tg,sup,df,tgt_hilo){
  cs = cs_gn(tg,sup,df)
  
  #had
  h = sum(cs[lower.tri(cs)])
  a = sum(cs[upper.tri(cs)])
  d = sum(diag(cs))
  
  #hilo
  u = soccerutility.hilo_under(cs,tgt_hilo)
  
  return(c(h, d, a, u, 1 - u))  
}

soccerutility.translate_for_goal = function(cs_matrix,score){
  
  r1 = nrow(cs_matrix) + score[1]
  c1 = ncol(cs_matrix) + score[2]
  
  x = matrix(
    nrow=r1,
    ncol=c1,
    data = 0);
  x[((1+score[1]):r1),((1+score[2]):c1)] = cs_matrix;
  
  x;
  
}
#returns an array of [home true,draw true, away true,hilo lines, hilo under true, hilo over true]
soccerutility.output_mkts_inplay = function(cs_gn,tg,sup,df,tgt_hilos,score){
  cs = soccerutility.cs_gn(tg,sup,df)
  
  cs = soccerutility.translate_for_goal(cs,score);
  
  #had
  h = sum(cs[lower.tri(cs)])
  a = sum(cs[upper.tri(cs)])
  d = sum(diag(cs))
  
  # nts;
  nts_n = cs[score[1]+1,score[2]+1]
  nts_h = (((tg + sup) / 2) / tg ) * (1 - nts_n);
  nts_a = 1 - nts_n - nts_h;
  
  
  # dictionary;
  ret = list()
  ret[['HAD']] = list('1'=1/h,'2'=1/a,'X'=1/d);
  ret[['NTS']] = list('1'=1/nts_h,'2'=1/nts_a,'N'=1/nts_n);
  
  #hilo
  for(i in 1:length(tgt_hilos)){
    u = soccerutility.hilo_under_detail(cs,tgt_hilos[i])
    ret[[paste0('HILO:',tgt_hilos[i])]] = list('L'=1/u[[1]][1],'H'=1/(1-u[[1]][1]),'D'=u[[2]][2])
  }
  
  #crs;
  scrs = list(
    c(1,0),
    c(2,0),
    c(2,1),	
    c(3,0),
    c(3,1),			
    c(3,2),	
    c(4,0),	
    c(4,1),	
    c(4,2),	
    c(5,0),	
    c(5,1),	
    c(5,2))	
  cs_h=0;
  cs_d=0;
  cs_a=0;
  for(i in 1:length(scrs)){
    b = scrs[[i]]
    ret[['CRS']][[paste0(b,collapse = ':')]] = 1/cs[b[1] + 1,b[2] + 1];
    cs_h = cs_h + cs[b[1] + 1,b[2] + 1];
    b = rev(b);
    ret[['CRS']][[paste0(b,collapse = ':')]] = 1/cs[b[1] + 1,b[2] + 1];
    cs_a = cs_a + cs[b[1] + 1,b[2] + 1];
  }
  for(i in 0:3){
    b= c(i,i)
    ret[['CRS']][[paste0(b,collapse = ':')]] = 1/cs[b[1] + 1, b[2]+1]
    cs_d = cs_d + cs[b[1] + 1, b[2]+1]
  }
  
  ret[['CRS']][['HomeOthers']] = 1/(h - cs_h);
  ret[['CRS']][['DrawOthers']] = 1/(d - cs_d);
  ret[['CRS']][['AwayOthers']] = 1/(a - cs_a);
  
  return(ret)
  
}

#returns an array of [home true,draw true, away true,hilo line, hilo under true, hilo over true]
soccerutility.output_mkts_hcp_hilo = function(cs_gn,tg,sup,df,tgt_hcp,tgt_hilo){
  cs = soccerutility.cs_gn(tg,sup,df)
  
  #had
  h = soccerutility.hcp_home(cs,tgt_hcp)
  
  #hilo
  u = soccerutility.hilo_under(cs,tgt_hilo)
  
  return(c(1/h,1/(1-h),1/u,1/(1-u)))
  
}

#returns an array of [home true,draw true, away true,hilo line, hilo under true, hilo over true]
soccerutility.output_mkts_hcp_hilo_bin = function(cs_gn,tg,sup,df,trials,tgt_hcp,tgt_hilo){
  cs = soccerutility.cs_gn(tg,sup,df,trials)
  
  #had
  h = soccerutility.hcp_home(cs,tgt_hcp)
  
  #hilo
  u = soccerutility.hilo_under(cs,tgt_hilo)
  
  return(c(1/h,1/(1-h),1/u,1/(1-u)))
  
}

#returns probability of under tgt_hilo
soccerutility.hilo_under = function(cs,line){
  
  lineid = as.integer(line / 0.25) %% 4
  
  if(lineid == 0){
    
    u = sum(cs[(row(cs) + col(cs) - 2)  <  as.integer(line)])
    d = sum(cs[(row(cs) + col(cs) - 2) ==  as.integer(line)])   
    x = u / (u + (1 - d - u))
    
  }
  else if(lineid == 1){
    
    u = sum(cs[(row(cs) + col(cs) - 2) <  as.integer(line)])
    d = sum(cs[(row(cs) + col(cs) - 2) ==  as.integer(line)])   
    x = (u + d / 2.0) / (1.0 - d / 2.0)         
    
  }
  else if(lineid == 2){
    
    u = sum(cs[(row(cs) + col(cs) - 2) <=  as.integer(line)])
    x = u
    
  }
  else{
    
    u = sum(cs[(row(cs) + col(cs) - 2)  <  as.integer(line) + 1])
    d = sum(cs[(row(cs) + col(cs) - 2) ==  as.integer(line) + 1])   
    x = u / (1 - d / 2.0)    
    
  }  
  
  return (x)     
  
}

soccerutility.hilo_under_detail = function(cs,line){
  
  lineid = as.integer(line / 0.25) %% 4
  d=0;
  if(lineid == 0){
    
    u = sum(cs[(row(cs) + col(cs) - 2)  <  as.integer(line)])
    d = sum(cs[(row(cs) + col(cs) - 2) ==  as.integer(line)])   
    x = u / (u + (1 - d - u))
    
  }
  else if(lineid == 1){
    
    u = sum(cs[(row(cs) + col(cs) - 2) <  as.integer(line)])
    d = sum(cs[(row(cs) + col(cs) - 2) ==  as.integer(line)])   
    x = (u + d / 2.0) / (1.0 - d / 2.0)         
    
  }
  else if(lineid == 2){
    
    u = sum(cs[(row(cs) + col(cs) - 2) <=  as.integer(line)])
    x = u
    
  }
  else{
    
    u = sum(cs[(row(cs) + col(cs) - 2)  <  as.integer(line) + 1])
    d = sum(cs[(row(cs) + col(cs) - 2) ==  as.integer(line) + 1])   
    x = u / (1 - d / 2.0)    
    
  }  
  if(!exists('d')){
    d = 0.0;
  }
  return (list(x,c(u,d,1-u-d)))
  
}

soccerutility.hilo_under_simple = function(u,d,h,line)
{
  lineid = as.integer(line / 0.25) %% 4
  
  if(lineid == 0){
    
    x = u / (u + (1 - d - u))
    
  }
  else if(lineid == 1){
    
    x = (u + d / 2.0) / (1.0 - d / 2.0)         
    
  }
  else if(lineid == 2){
    
    x = u
    
  }
  else{
    
    x = u / (1 - d / 2.0)    
    
  }  
  
  return (x)  
}

#returns probability of under tgt_hilo
soccerutility.hcp_home = function(cs,line){
  
  lineid = as.integer(abs(line / 0.25)) %% 4  
  
  if(lineid == 0){
    
    #line = line * -1
    d = sum(off.diag(cs,as.integer(line)))
    h = sum(off.diag.seg(cs,as.integer(line) - 1))
    
    x = h / (h + (1 - d - h))
    
  }#1/4 ball
  else if(lineid == 1){
    
    if(line > 0){
      
      d = sum(off.diag(cs,(as.integer(line - 0.25))))
      h = sum(off.diag.seg(cs,((as.integer(line - 0.25))) - 1))
      x = (h + d / 2.0) / (1.0 - d / 2.0)         
      
    }
    else{      
      
      d = sum(off.diag(cs,(as.integer(line + 0.25))))
      h = sum(off.diag.seg(cs,((as.integer(line + 0.25))) - 1))
      x = h / (1.0 - (d / 2.0))
      
    }    
  }#1/2 ball
  else if(lineid == 2){
    
    h = sum(off.diag.seg(cs,as.integer(line - 0.5)))
    x = h
    
  }#3/4 ball
  else{
    
    if(line > 0){
      
      d = sum(off.diag(cs,(as.integer(line + 0.25))))  
      h = sum(off.diag.seg(cs,((as.integer(line + 0.25))) - 1))   
      x = h / (1 - d / 2.0) 
      
    }else{
      
      d = sum(off.diag(cs,(as.integer(line - 0.25))))
      h = sum(off.diag.seg(cs,((as.integer(line - 0.25))) - 1))
      x = (h + d / 2.0) / (1.0 - d / 2.0)    
      
      
    }  
    
  }  
  
  return (x)     
  
}

#rev offset diag
off.rev.diag <- function(m, i = 0) m[ (row(m) + col(m) - 1) %% ncol(m) == i ]

#offset diag
off.diag <- function(m, i = 0) m[ row(m) +  i == col(m)]

off.diag.seg <- function(m, i = 0) m[row(m) + i >= col(m)]

#returns the squared error between true prices
#tgts = [hwin true, dwin true, awin true, hilo under true, hilo over true]
soccerutility.target = function(cs_gn,friday,hilo_line,tgts){
  out = soccerutility.output_mkts(cs_gn,friday[1],friday[2],friday[3],hilo_line)
  err = sum(((out - tgts) ^ 2)) ^ 0.5
  
  return (err)
}

#tgts = [hwin,awin,under,over]
soccerutility.target_hcp_hilo = function(cs_gn,par,hilo_line,hcp_line,tgts){
  out = soccerutility.output_mkts_hcp_hilo(cs_gn,par[1],par[2],1,hcp_line,hilo_line)
  err = sum(((out - tgts) ^ 2)) ^ 0.5
  
  return (err)
}

#tgts = [hwin,awin,under,over]
soccerutility.target_hcp_hilo_df = function(cs_gn,par,hilo_line,hcp_line,tgts){
  out = soccerutility.output_mkts_hcp_hilo(cs_gn,par[1],par[2],par[3],hcp_line,hilo_line)
  err = sum(((out - tgts) ^ 2)) ^ 0.5
  
  return (err)
}

soccerutility.target_hcp_hilo_df_fixed = function(cs_gn,par,df_fix,hilo_line,hcp_line,tgts){
  out = soccerutility.output_mkts_hcp_hilo(cs_gn,par[1],par[2],df_fix,hcp_line,hilo_line)
  err = sum(((out - tgts) ^ 2)) ^ 0.5
  
  return (err)
}

soccerutility.target_hcp_hilo_bin = function(cs_gn,par,trials,hilo_line,hcp_line,tgts){
  out = soccerutility.output_mkts_hcp_hilo_bin(cs_gn,par[1],par[2],1,trials,hcp_line,hilo_line)
  err = sum(((out - tgts) ^ 2)) ^ 0.5
  
  return (err)
}

soccerutility.cop_cs_gen_wrapper = function(tg,sup,df){
  #ignore draw factor
  par[0] = df
  par[1] = 1.0
  
  e1 = (tg + sup)/2.0
  e2 = tg - e1
  return(Score(e1,e2,par,90, Binom, CumBinom, Frank,CumFrank))
  
}

soccerutility.prob_mult = function(p,m){  
  return (p*m/((1-p) + (p*m)))  
}

soccerutility.mod = function(win_rat,m){
  x = 1.0
  if(win_rat < 0.5)
  {
    if(win_rat < m)
    {
      x = 0.0
    }
    else
    {
      x = (win_rat - m) / (0.5 - m)
    }  
  }
  return(x)
}

soccerutility.fitWrapper = function(cs_gen,had,hilo,start = c(2.5,0.1,1.0),returnError = FALSE){
  
  x = optim(start,fn=soccerutility.target,cs_gn= cs_gen,hilo_line=hilo[1],tgts=c(had[1],had[2],had[3],hilo[2],hilo[3]))
  
  if(returnError==FALSE)
  {
    return(c( x$par, x$value))
  }
  else
  {
    z = soccerutility.output_mkts(cs_gn=cs_gen,tg=x$par[1],sup=x$par[2],df= x$par[3],tgt_hilo=hilo[1])
    return(c(x,paste(z, collapse = ':')))
  }
}

soccerutility.fitWrapper_HcpHilo = function(cs_gen,hcp,hilo,start = c(2.5,0.1,1.0)){
  x = optim(start,fn=soccerutility.target_hcp_hilo,cs_gn= cs_gen,hcp_line = hcp[1],hilo_line=hilo[1],tgts=c(hcp[2],hcp[3],hilo[2],hilo[3]))
  return(c(x$par,x$value))
  
}

soccerutility.fitWrapper_HcpHilo_bin = function(cs_gen,trials,hcp,hilo,start = c(2.5,0.1,1.0)){
  x = optim(start,fn=soccerutility.target_hcp_hilo_bin,cs_gn= cs_gen,trials = trials,hcp_line = hcp[1],hilo_line=hilo[1],tgts=c(hcp[2],hcp[3],hilo[2],hilo[3]))
  return(c(x$par,x$value))  
}

# Adjusts a given TG,SUP,DF, to an equivalent TG,SUP with a desired draw factor.
soccerutility.fix_par_to_df = function(tg0,sup0,df0,df_tgt){
  hilo_line = as.integer(round(tg0));
  hcp_line = as.integer(round(sup0));
  fix = soccerutility.output_mkts_hcp_hilo(cs_poisson_df,tg0,sup0,df0,hcp_line,hilo_line);
  start = c(tg0,sup0)
  x = optim(start,
            fn=soccerutility.target_hcp_hilo_df_fixed,
            cs_gn= soccerutility.cs_poisson_df,
            hcp_line = hcp_line,
            hilo_line=hilo_line,
            df_fix=df_tgt,
            tgts=fix,control=list(maxit=10000))
  return(c(x$par[1],x$par[2],df_tgt))
  
}


soccerutility.fitExpectancy = function(lines_and_under) {
  #lines_and_under = data.frame(lines_and_under)
  if(!is.null(dim(lines_and_under))) {
    par = optim(as.numeric(lines_and_under[1,1]), fn=soccerutility.expectancyErrorMultipleLines, lines_and_under=lines_and_under,method = 'Brent',lower = 0.1,upper=20)$par
  } else {
    par = optim(lines_and_under[1], fn=soccerutility.expectancyErrorSingleLine, lines_and_under=lines_and_under,method = 'Brent',lower = 0.1,upper=20)$par
  }
  return(par)
}

soccerutility.fitExpectancyNB = function(lines_and_under) {
  #lines_and_under = data.frame(lines_and_under)
  if(!is.null(dim(lines_and_under))) {
    par = optim(par = c(as.numeric(lines_and_under[1,1]),as.numeric(lines_and_under[1,1])), fn=soccerutility.expectancyErrorMultipleLinesNB, lines_and_under=lines_and_under)$par
  } else {
    par = optim(c(as.numeric(lines_and_under[1]),as.numeric(lines_and_under[1])), fn=soccerutility.expectancyErrorSingleLineNB, lines_and_under=lines_and_under)$par
  }
  return(par)
}
soccerutility.fitExpectancyNBFixedTheta = function(lines_and_under, theta) {
  #lines_and_under = data.frame(lines_and_under)
  if(!is.null(dim(lines_and_under))) {
    par = optim(par = c(as.numeric(lines_and_under[1,1])), fn=soccerutility.expectancyErrorMultipleLinesNBFixedTheta, lines_and_under=lines_and_under,theta = theta,method = 'Brent',lower = 0.1,upper=30)$par
  } else {
    par = optim(c(as.numeric(lines_and_under[1])), fn=soccerutility.expectancyErrorSingleLineNBFixedTheta, lines_and_under=lines_and_under, theta,method = 'Brent',lower = 0.1,upper=30)$par
  }
  return(par)
}
soccerutility.expectancyErrorMultipleLines = function(lines_and_under, par) {
  ret_val = ppois(as.integer(floor(lines_and_under[,1])), par)
  error = (ret_val - lines_and_under[,2])^2
  return(sum(error)^0.5)
}
soccerutility.expectancyErrorSingleLine = function(lines_and_under, par) {
  ret_val = ppois(as.integer(floor(lines_and_under[1])), par)
  error = (ret_val - lines_and_under[2])^2
  return(error^0.5)
}
soccerutility.expectancyErrorMultipleLinesNB = function(lines_and_under, par) {
  ret_val = pnbinom(as.integer(floor(lines_and_under[,1])),mu =  par[1], size = par[2])
  error = (ret_val - lines_and_under[,2])^2
  return(sum(error)^0.5)
}
soccerutility.expectancyErrorSingleLineNB = function(lines_and_under, par) {
  ret_val = pnbinom(as.integer(floor(lines_and_under[1])),mu =  par[1], size = par[2])
  error = (ret_val - lines_and_under[2])^2
  return(error^0.5)
}
soccerutility.expectancyErrorMultipleLinesNBFixedTheta = function(lines_and_under, par, theta) {
  ret_val = pnbinom(as.integer(floor(lines_and_under[,1])),mu =  par[1], size = theta)
  error = (ret_val - lines_and_under[,2])^2
  return(sum(error)^0.5)
}
soccerutility.expectancyErrorSingleLineNBFixedTheta = function(lines_and_under, par ,theta) {
  ret_val = pnbinom(as.integer(floor(lines_and_under[1])),mu =  par[1], size = theta)
  error = (ret_val - lines_and_under[2])^2
  return(error^0.5)
}

soccerutility.expectancyMarketPoisson = function(lines_set, par) {
  ret_val = ppois(0:20, par)
  ret_frame = (data.frame(lines = 0:20 + 0.5, Under = ret_val, Over = 1 - ret_val))
  if(!is.null(lines_set)) {
    ret_frame = ret_frame[ret_frame$lines %in% lines_set,]
  }
  return(ret_frame)
}
soccerutility.expectancyMarketNB = function(lines_set, par) {
  ret_val = pnbinom(0:20, mu=par[1],size=par[2])
  ret_frame = (data.frame(lines = 0:20 + 0.5, Under = ret_val, Over = 1 - ret_val))
  if(!is.null(lines_set)) {
    ret_frame = ret_frame[ret_frame$lines %in% lines_set,]
  }
  return(ret_frame)
}