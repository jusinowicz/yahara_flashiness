
#model_form [[1]] = "level ~ s(time, bs = \"cr\", k = 100)"
# model_form [[1]] = "level ~ s(time, bs = \"cr\", k = 100)+
#     s(level1,bs=\"cr\",k=6)+s(level2,bs=\"cr\",k=6)+s(level3,bs=\"cr\",k=6)+
#     s(level4,bs=\"cr\",k=6)+s(level5,bs=\"cr\",k=6)+
#     s(rn,bs=\"cr\",k=6)+s(rn1,bs=\"cr\",k=6)+
#     s(rn2,bs=\"cr\",k=6)+s(rn3,bs=\"cr\",k=6)+s(rn4,bs=\"cr\",k=6)+
#     te(rn,time,k=20)+te(rn1,time,k=20)+te(rn2,time,k=20)+
#     te(rn3,time,k=20)+te(rn4,time,k=20)+te(rn,rn1,k=20)+
#     te(rn1,rn2,k=20)+te(rn2,rn3,k=20)"

# model_form [[2]] = "level ~ s(time, bs = \"cr\", k = 100)+
#     s(level1,bs=\"cr\",k=6)+s(level2,bs=\"cr\",k=6)+s(level3,bs=\"cr\",k=6)+
#     s(level4,bs=\"cr\",k=6)+s(level5,bs=\"cr\",k=6)+s(level6,bs=\"cr\",k=6)+
#     s(rn,bs=\"cr\",k=6)+s(rn1,bs=\"cr\",k=6)+
#     s(rn2,bs=\"cr\",k=6)+s(rn3,bs=\"cr\",k=6)+s(rn4,bs=\"cr\",k=6)+
#     te(rn,time,k=20)+te(rn1,time,k=20)+te(rn2,time,k=20)+
#     te(rn3,time,k=20)+te(rn4,time,k=20)+te(rn,rn1,k=20)+
#     te(rn1,rn2,k=20)+te(rn2,rn3,k=20)"


# model_form [[1]] = "level ~ s(level, bs = \"ar\", m = 4)+
#     s(rn,bs=\"cr\",k=6)+s(rn1,bs=\"cr\",k=6)+
#     s(rn2,bs=\"cr\",k=6)+s(rn3,bs=\"cr\",k=6)+
#     ti(rn,rn1,k=20)+ti(rn1,rn2,k=20)+ti(rn2,rn3,k=20)"

# model_form [[2]] = "level ~ 
#   s(rn,bs=\"cr\",k=6)+s(rn1,bs=\"cr\",k=6)+
#     s(rn2,bs=\"cr\",k=6)+s(rn3,bs=\"cr\",k=6)+
#     ti(rn,rn1,k=20)+ti(rn1,rn2,k=20)+ti(rn2,rn3,k=20)"

# model_form [[1]] = "level ~
#     s(level1,bs=\"cr\",k=6)+s(level2,bs=\"cr\",k=6)+s(level3,bs=\"cr\",k=6)+
#     s(level4,bs=\"cr\",k=6)+s(rn1,bs=\"cr\",k=6)+
#     s(rn2,bs=\"cr\",k=6)+s(rn3,bs=\"cr\",k=6)+s(rn4,bs=\"cr\",k=6)+
#     te(rn,time,k=20)+te(rn1,time,k=20)+te(rn2,time,k=20)+
#     te(rn3,time,k=20)+te(rn4,time,k=20)"

# model_form [[2]] = "level ~ 
#     s(level1,bs=\"cr\",k=6)+s(level2,bs=\"cr\",k=6)+s(level3,bs=\"cr\",k=6)+
#     s(level4,bs=\"cr\",k=6)+s(rn1,bs=\"cr\",k=6)+
#     s(rn2,bs=\"cr\",k=6)+s(rn3,bs=\"cr\",k=6)+s(rn4,bs=\"cr\",k=6)+
#     te(rn,time,k=20)+te(rn1,time,k=20)+te(rn2,time,k=20)+
#     te(rn3,time,k=20)+te(rn4,time,k=20)"


# model_form [[1]] = "level ~ s(time, bs = \"cr\", k = 100)+
#     s(level1,bs=\"cr\",k=6)+s(level2,bs=\"cr\",k=6)+
#     s(rn1,bs=\"cr\",k=6)+
#     s(rn2,bs=\"cr\",k=6)+s(rn3,bs=\"cr\",k=6)+
#     te(rn,time,k=20)+te(rn1,time,k=20)+te(rn2,time,k=20)+
#     te(rn3,time,k=20)"

# model_form [[2]] = "level ~ s(time, bs = \"cr\", k = 100)+
#     s(level1,bs=\"cr\",k=6)+s(level2,bs=\"cr\",k=6)+s(level3,bs=\"cr\",k=6)+
#     s(rn1,bs=\"cr\",k=6)+
#     s(rn2,bs=\"cr\",k=6)+s(rn3,bs=\"cr\",k=6)+
#     te(rn,time,k=20)+te(rn1,time,k=20)+te(rn2,time,k=20)+
#     te(rn3,time,k=20)"

# model_form [[1]] = "level ~ s(time, bs = \"cr\", k = 100)+
#     s(rn,bs=\"cr\",k=6)+ s(rn2,bs=\"cr\",k=6)+ ti(rn,time,k=20)+ ti(rn2,time,k=20)"

# model_form [[1]] = "level ~ s(time, bs = \"cr\", k = 100)+
#     s(rn,bs=\"cr\",k=6)+ ti(rn,time,k=20)"

# model_form [[1]] = "level ~ s(time, bs = \"cr\", k = 100)+
#     s(rn,bs=\"cr\",k=6)+s(rn2,bs=\"cr\",k=6)+s(rn3,bs=\"cr\",k=6)+ 
#     ti(rn,time,k=20)+ti(rn2,time,k=20)+ti(rn3,time,k=20)"

# model_form [[2]] = "level ~ s(time, bs = \"cr\", k = 100)+
#     s(rn,bs=\"cr\",k=6) + ti(rn,time,k=20)"


predictFlashGAM = function(lake_data, fut_precip){

    #Where the fitted model coefficients and Lp matrix live
    model_files = list.files("./")
    model_true = grepl("*GAM*.*var|*var.*GAM*", model_files)

    #Which are the model files? 
    model_files = model_files[model_true == TRUE ]
    n_files = length(model_files)
    #Loop and load the files 
    for ( n in 1:n_files ){ 
      load(paste(model_files[n]) )
    }

    #How many days are we forecasting? 
    n_days = dim(fut_precip)[1]

    #Most current time step
    ntime = (tail(lake_data[[1]],1))$time

    #Predicted lake levels 
    pred_lakes = vector("list",n_lakes)
    pred_lakes_ar = vector("list",n_lakes)

    #Build the new data set for prediction and make predictions:
    for (n in 1:n_lakes){ 

      #AR order of rain and lake level
      ar_lake = grep("level", (colnames(lake_data[[n]])))
      ar_lake = ar_lake[-1]
      ar_rain = grep("rn", (colnames(lake_data[[n]]))) 
      ar_rain = ar_rain[-1]
      l_arl = length (ar_lake)
      l_arr = length (ar_rain)

      #Which AR is larger? 
      if(l_arl>l_arr){ lar = l_arl}else{lar = l_arr}
      
      #Get the last section of data table for lags
      lt = tail(lake_data[[n]], l_arl)
      
      #Look for an NA in most recent rn, this happens
      if(sum(is.na(lt$rn)) > 0){  
        lt$rn[is.na(lt$rn)] = mean(lt$rn,na.rm=T)
      }

      #Version 1: An interative prediction approach
      #The start of the new data set for prediction with 
      #the first new day
      lt_tmp = as.data.frame(c(ntime+1, NA, lt[l_arl,2:(l_arl+1)],
        fut_precip[1,2],
        lt[l_arl,(l_arl+3):(l_arl+2+l_arr) ] ))
      colnames(lt_tmp) = colnames(lt)
      lt_new = rbind( lt,lt_tmp) 
      
      #Initialize new data set
      lt_use = lt_new[l_arl+1,]
      lt_save = NULL

      #Temporarily store predicted lake level and SE
      pr_tmp = matrix(0, n_days, 4 )
      pr_tmp[,1] = ntime+(1:n_days)
      pr_tmp[,2] = fut_precip$rn
      colnames(pr_tmp) = c("time", "rn", "level", "se")
      for (t in 1:n_days){

        pt = predict(lake_models[[n]], newdata=lt_use,se.fit=TRUE)
        pr_tmp[t,3] =pt$fit[1]
        pr_tmp[t,4] = pt$se[1]

        #Now update the lags in lt_use with data for this day, 
        #but don't do this for n_days
        if (t < n_days){ 
          lt_use$level = pr_tmp[t,3] #Replace NA with prediction
          lt_use = as.data.frame(c(ntime+t, NA, lt_use[1,2:(l_arl+1)],
          fut_precip[t+1,2],
          lt_use[1,(l_arl+3):(l_arl+2+l_arr) ] ))
          colnames(lt_use) = colnames(lt)
        }else{   }
      
      }

      pred_lakes[[n]]  = as.data.frame(pr_tmp)

      #Version 2: Use ar() to predict just the lake level then
      #a normal predict() using the interpolated future lake
      #level 
      ll_ar = ar(lake_data[[n]]$level)
      ll_tmp1 = as.matrix(predict(ll_ar, n.ahead = n_days)$pred)
      
      #Now append this to a larger data set to make all of the lags:       
      ltr = tail(lake_data[[n]], l_arr)
      #Look for an NA in most recent rn, this happens
      if(sum(is.na(ltr$rn)) > 0){  
        ltr$rn[is.na(ltr$rn)] = mean(lt$rn,na.rm=T)
      }

      #Get just the level and rn data, then make the lags
      ll_tmp2 = data.frame(level=c(ltr$level, ll_tmp1))
      rn_tmp = data.frame(rn = c(ltr$rn,fut_precip$rn))
      ll_ar_data = make.flashiness.object(ll_tmp2,rn_tmp,lags, auto=F, orders = l_arl )

      #Only want the last n_days of this for prediction
      lt_use = tail(ll_ar_data, n_days)
      pt = predict(lake_models3[[n]], newdata=lt_use,se.fit=TRUE)
      #pt = predict(lake_models[[n]], newdata=lake_data2[[n]][t1:t2,],se.fit=TRUE)


      pred_lakes_ar[[n]] = data.frame(time = (ntime+1):(ntime+n_days) , rn = lt_use$rn, 
        level = c(pt$fit)+c(ll_tmp1), se = pt$se.fit )
       
      pt2= data.frame(time = (ntime+1):(ntime+n_days) , rn = lt_use$rn, 
        level = pt$fit, se = pt$se.fit )
  
  }

  t1 = 1000
  t2 = 1100     

  fut_precip = lake_data2[[n]][t1:t2,c("time","rn")]
  lt = lake_data[[n]][(t1-l_arl):t1,]

  ll_ar = ar(lake_data[[n]][1:t1,]$level)
  ltr = lake_data[[n]][(t1-l_arl):t1,]

      pt = predict(lake_models[[n]], newdata=lake_data2[[n]],se.fit=TRUE)
        pt2= data.frame(time = (ntime+1):(ntime+n_days) , rn = lt_use$rn[(ntime+1):(ntime+n_days) ], 
        level = pt$fit[(ntime+1):(ntime+n_days) ], se = pt$se.fit[(ntime+1):(ntime+n_days) ] )

      a1=predict(lake_models[[n]],newdata=lake_data[[n]][t1:t2,],se.fit=TRUE)

     # a2 = data.frame(time =lake_data[[n]][,1], 
     #    level = lake_data[[n]][,"level"], 
     #    level_p = a1$fit, se = a1$se )

      a2 = data.frame(time =lake_data[[n]][t1:t2,1], 
        level = lake_data[[n]][t1:t2,"level"], 
        level_p = a1$fit, se = a1$se )

      ggplot( ) + geom_line(data = a2, aes(x = time, y=level_p)) +
      geom_ribbon(data = a2, aes(x = time, ymin = level_p-se, ymax = level_p+se), alpha = 0.2)+
      geom_point(data = a2, aes(x=time, y=level),color="red")

      ggplot( ) + geom_line(data = pred_lakes[[n]], aes(x = time, y=level)) +
      geom_ribbon(data =  pred_lakes[[n]], aes(x = time, ymin = level-se, ymax = level+se), alpha = 0.2)+
      #geom_line(data = pred_lakes_ar[[n]], aes(x = time, y=level),col="blue") +
      #geom_line(data = pt2, aes(x = time, y=level),col="blue") +
      #geom_ribbon(data =  pred_lakes_ar[[n]], aes(x = time, ymin = level-se, ymax = level+se), alpha = 0.2)+
      geom_point(data = lake_data2[[n]][t1:(t1+101),], aes(x=time, y=level),color="red")  
        


 for (t in 1:n_days){

        pt = predict(lake_models4[[n]], newdata=lt_use,se.fit=TRUE, type ="response")
        ll_ar = ar(ld_use$level)
        ll_tmp1 = as.matrix(predict(ll_ar, n.ahead = 1)$pred)

        pr_tmp[t,3] =pt$fit[1] + ll_tmp1[1]
        pr_tmp[t,4] = pt$se[1]

        #Now update the lags in lt_use with data for this day, 
        #but don't do this for n_days
        if (t < n_days){ 
          lt_use$level = pr_tmp[t,3] #Replace NA with prediction
          ld_use = rbind(ld_use, lt_use)
          lt_use = as.data.frame(c(ntime+t, NA, lt_use[1,2:(l_arl+1)],
          fut_precip[t+1,2],
          lt_use[1,(l_arl+3):(l_arl+2+l_arr) ] ))

          colnames(lt_use) = colnames(lt)
        }else{   }
      
      }



      #Version 2: Use ar() to predict just the lake level then
      #a normal predict() using the interpolated future lake
      #level 
      ll_ar = ar(lake_data[[n]]$level)
      ll_tmp1 = as.matrix(predict(ll_ar, n.ahead = n_days)$pred)
      
      #Now append this to a larger data set to make all of the lags:       
      ltr = tail(lake_data[[n]], l_arr)
      #Look for an NA in most recent rn, this happens
      if(sum(is.na(ltr$rn)) > 0){  
        ltr$rn[is.na(ltr$rn)] = mean(lt$rn,na.rm=T)
      }

      #Get just the level and rn data, then make the lags
      ll_tmp2 = data.frame(level=c(ltr$level, ll_tmp1))
      rn_tmp = data.frame(rn = c(ltr$rn,fut_precip$rn))
      ll_ar_data = make.flashiness.object(ll_tmp2,rn_tmp,lags, auto=F, orders = l_arl )

      #Only want the last n_days of this for prediction
      lt_use = tail(ll_ar_data, n_days)
      pt = predict(lake_models4[[n]], newdata=lt_use,se.fit=TRUE)
      #pt = predict(lake_models[[n]], newdata=lake_data2[[n]][t1:t2,],se.fit=TRUE)


      pred_lakes_ar[[n]] = data.frame(time = (ntime+1):(ntime+n_days) , rn = lt_use$rn, 
        level = c(pt$fit)+c(ll_tmp1), se = pt$se.fit )
       
      pt2= data.frame(time = (ntime+1):(ntime+n_days) , rn = lt_use$rn, 
        level = pt$fit, se = pt$se.fit )

      #Version 3: First predict the lake level, then the AR. 
      #Only want the last n_days of this for prediction

      #Get just the level and rn data, then make the lags
      ll_tmp2 = data.frame(level=c(ltr$level, ll_tmp1))
      rn_tmp = data.frame(rn = c(ltr$rn,fut_precip$rn))
      ll_ar_data = make.flashiness.object(ll_tmp2,rn_tmp,lags, auto=F, orders = l_arl )

      lt_use = tail(ll_ar_data, n_days)
      pt = predict(lake_models4[[n]], newdata=lt_use,se.fit=TRUE)

      Use ar() to predict just the lake level then
      #a normal predict() using the interpolated future lake
      #level 
      ll_ar = ar(lake_data[[n]]$level)
      ll_tmp1 = as.matrix(predict(ll_ar, n.ahead = n_days)$pred)
      
      #Now append this to a larger data set to make all of the lags:       
      ltr = tail(lake_data[[n]], l_arr)
      #Look for an NA in most recent rn, this happens
      if(sum(is.na(ltr$rn)) > 0){  
        ltr$rn[is.na(ltr$rn)] = mean(lt$rn,na.rm=T)
      }


     
      #pt = predict(lake_models[[n]], newdata=lake_data2[[n]][t1:t2,],se.fit=TRUE)


      pred_lakes_ar[[n]] = data.frame(time = (ntime+1):(ntime+n_days) , rn = lt_use$rn, 
        level = c(pt$fit)+c(ll_tmp1), se = pt$se.fit )
       
      pt2= data.frame(time = (ntime+1):(ntime+n_days) , rn = lt_use$rn, 
        level = pt$fit, se = pt$se.fit )


      
###############################################################################
# NOT IMPLEMENTED: 
# predictFlashGAM_LP uses the Lp Matrix of a fitted GAM to predict
#
#This is the version of predictFlashGAM that would be used if the model 
#produced in the fitting process was too massive to store and recall
#efficiently from memory. 
###############################################################################

predictFlashGAM_LP = function(lake_data, fut_precip){

    #Where the fitted model coefficients and Lp matrix live
    model_files = list.files("./")
    model_true = grepl("*GAM*.*var|*var.*GAM*", model_files)

    #Which are the model files? 
    model_files = model_files[model_true == TRUE ]
    n_files = length(model_files)
    #Loop and load the files 
    for ( n in 1:n_files ){ 
      load(paste(model_files[n]) )
    }

    #How many days are we forecasting? 
    n_days = dim(fut_precip)[1]

    #Most current time step
    ntime = dim(lake_data[[1]])[1]

    #This section will break down the formula and extract two 
    #key pieces of info: the number of smooth terms and the 
    #number of knots for each 
    model_clean = vector("character",n_lakes)
    model_ks = vector("list",n_lakes)

    #a1 = ts( as.matrix(fut_precip[,2]), start=real_start[[n]], frequency=freq )
    
    # fp_table[[n]] = make.flashiness.object(lake.tmp, rn.tmp, lags)

    for (n in 1:n_lakes){ 

      #AR order of rain and lake level
      ar_lake = grep("level", (colnames(lake_data[[n]])))
      ar_lake = ar_lake[-1]
      ar_rain = grep("rn", (colnames(lake_data[[n]]))) 
      ar_rain = ar_rain[-1]
      l_arl = length (ar_lake)
      l_arr = length (ar_rain)

      #Which AR is larger? 
      if(l_arl>l_arr){ lar = l_arl}else{lar = l_arr}
      
      #Get the last section of data table for lags
      lt = tail(lake_data[[n]], l_arl)
 
      #The start of the new data set for prediction with 
      #the first new day
      lt_tmp = as.data.frame(c(ntime+1, NA, lt[l_arl,2:(l_arl+1)],
        fut_precip[1,2],
        lt[l_arl,(l_arl+3):(l_arl+2+l_arr) ] ))
      colnames(lt_tmp) = colnames(lt)
      lt_new = rbind( lt,lt_tmp) 
      lt_use = lt_new[, -2]

      #Figure out the smooth terms 
      n_terms = length(model_smooths[[n]])
      all_ks = colnames(models_Xp[[n]])
      model_ks[[n]] = matrix(0,n_terms,1)

      for(t in 1:n_terms){

        sm_tmp = model_smooths[[n]][t] 
        ks_tmp = grepl(
        glob2rx(paste(sm_tmp,".*",sep="")), 
        all_ks)
        model_ks[[n]][t] = sum(ks_tmp) 

      }

      n_smooth = sum(model_ks[[n]])

      #Find the average distance between samples:
      ldiffs = apply(lake_data[[n]],2,diff)  
      m_ldiff = apply(abs(ldiffs), 2, mean,na.rm=T) 
      
 
      
      #Do each time-step sequentially: 
      for (f in 1:n_days){

        x0 = 1         ## intercept column

        #Loop through smooth terms:
        for ( j in 0:(n_terms-1) ) {
          dx = m_ldiff[j+2]
          #Get the set of columns for the smooth
          if(j == 0) { cst = 0 }else{
            cst = sum(model_ks[[n]][1:(j)])
          }
          lcols = 1+cst + 1:(model_ks[[n]][j+1])
          #Find the relevant rows
          lrows = floor(lt_use[ (l_arl+f),j+2]/dx) 
          w1 = (lt_use[(l_arl+f), j+2]-lrows*dx)/dx ## interpolation weights
          ## find approx. predict matrix row portion, by interpolation
          x0 = c(x0,models_Xp[[n]][lrows+2,lcols]*w1 + models_Xp[[n]][lrows+1,lcols]*(1-w1))
        }

        dim(x0)=c(1,dim(models_Xp[[n]])[2]) 
        fv = x0%*%models_coef[[n]]   ## evaluate and add offset
        se = sqrt(x0%*%models_Vp[[n]]%*%t(x0)) ## get standard error



   }
}

}


