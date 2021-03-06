library(shiny)
library(ggplot2)
library(extraDistr)

ui <- fluidPage(tabsetPanel(
  tabPanel(
    title = "Repartitie Binomiala",
    sliderInput(inputId = "nr_expbin", label = "Alege numarul de experimente", min = 1, max = 1000, value = 100),
    sliderInput(inputId = "probbin", label = "Alege probabilitatea", min = 0, max = 1, value = 0.5),
    numericInput(inputId = "a_bin", label = "a = ", value = 50, min = 0),
    numericInput(inputId = "b_bin", label = "b = ", value = 70, min = 0),
    plotOutput("masa_binomiala"),
    plotOutput("rep_binomiala"),
    plotOutput("probabilitati1bin")
  ),
  tabPanel(
    title = "Repartitie Hypergeometrica",
    sliderInput(inputId = "albehip", label = "Numarul de bile albe", min = 0, max = 100, value = 50),
    sliderInput(inputId = "negrehip", label = "Numarul de bile negre", min = 0, max = 100, value = 50),
    sliderInput(inputId = "khip", label = "Numarul de bile extrase", min = 0, max = 100, value = 30),
    numericInput(inputId = "a_hip", label = "a = ", value = 20, min = 0),
    numericInput(inputId = "b_hip", label = "b = ", value = 25, min = 0),
    plotOutput("masa_hipergeometrica"),
    plotOutput("rep_hipergeometrica"),
    plotOutput("probabilitati1hip")
  ),
  tabPanel(
    title = "Repartitie Exponentiala",
    sliderInput(inputId = "nr_expexp", label = "Alege numarul de experimente", min = 1, max = 100, value = 20),
    sliderInput(inputId = "rata", label = "Alege rata", min = 0, max = 1, value = 0.5),
    numericInput(inputId = "a_exp", label = "a = ", value = 10, min = 0),
    numericInput(inputId = "b_exp", label = "b = ", value = 15, min = 0),
    plotOutput("masa_exponentiala"),
    plotOutput("probabilitati1exp"),
  ),
  tabPanel(
    title = "Repartitie Poisson",
    sliderInput(inputId = "lambda", label = "Rata aparitiei: ", min = 1, max = 50, value = 10),
    numericInput(inputId = "a_pois", label = "a = ", value = 5, min = 0),
    numericInput(inputId = "b_pois", label = "b = ", value = 10, min = 0),
    plotOutput("masa_poisson"),
    plotOutput("rep_poisson"),
    plotOutput("P1poisson")
  ),
  tabPanel(
    title = "Repartitie Geometrica",
    sliderInput(inputId = "probgeom", label = "Alege probabilitatea: ", min = 0, max = 1, value = 0.5),
    numericInput(inputId = "a_geom", label = "a = ", value = 3, min = 0),
    numericInput(inputId = "b_geom", label = "b = ", value = 5, min = 0),
    plotOutput("masa_geometrica"),
    plotOutput("rep_geometrica"),
    plotOutput("probabilitati1geom"),
  ),
  tabPanel(
    title = "Repartitie Uniforma Discreta",
    sliderInput(inputId = "min_dunif", label = "Alege minimul: ", min = 1, max = 20, value = 5),
    sliderInput(inputId = "max_dunif", label = "Alege maximul: ", min = 1, max = 20, value = 15),
    numericInput(inputId = "a_dunif", label = "a = ", value = 9, min = 0),
    numericInput(inputId = "b_dunif", label = "b = ", value = 17, min = 0),
    plotOutput("masa_duniforma"),
    plotOutput("rep_duniforma"),
    plotOutput("probabilitati1dunif")
  ),
  tabPanel(
    title = "Repartitie Beta",
    sliderInput(inputId = "shape1_beta", label = "Alege Alpha: ", min = 0, max = 10, value = 2),
    sliderInput(inputId = "shape2_beta", label = "Alege Beta: ", min = 0, max = 10, value = 2),
    numericInput(inputId = "a_beta", label = "a = ", value = 0.2, min = 0,max=1),
    numericInput(inputId = "b_beta", label = "b = ", value = 0.5, min = 0, max=1),
    plotOutput("densitatea_beta"),
    plotOutput("probabilitati1beta")
  ),
  tabPanel(
    title = "Repartitie Gamma",
    sliderInput(inputId = "shape_gamma", label = "Shape : ", min = 0, max = 20, value = 7),
    sliderInput(inputId = "rata_gamma", label = "Rata: ", min = 0, max = 5, value = 2.5),
    numericInput(inputId = "a_gamma", label = "a = ", value = 3, min = 0,max=20),
    numericInput(inputId = "b_gamma", label = "b = ", value = 7, min = 0, max=20),
    plotOutput("densitatea_gamma"),
    plotOutput("probabilitati1gamma")
  ),
  tabPanel(
    title = "Reparitie Uniforma Continua",
    sliderInput(inputId ="min_unif", label = "Alege minimul", min = 0, max = 20, value = 5),
    sliderInput(inputId ="max_unif", label = "Alege maximul", min = 0, max = 20, value = 15),
    numericInput(inputId = "a_unif", label = "a = ", value = 10, min = 0),
    numericInput(inputId = "b_unif", label = "b = ", value = 15, min = 0),
    plotOutput("dens_uniforma"),
    plotOutput("rep_uniforma")
  ),
  tabPanel(
    title = "Reparitie Chi-Square",
    sliderInput(inputId = "chi_nr", label = "Bound", min = 10, max = 100, value = 30),
    sliderInput(inputId = "chi_df", label = "Degree of freedom", min = 0, max = 20, value = 3, step=0.1),
    sliderInput(inputId = "chi_cent", label = "Center", min = 0, max = 25, value = 0),
    numericInput(inputId = "chi_a", label = "a = ", value = 10, min = 0),
    numericInput(inputId = "chi_b", label = "b = ", value = 18, min = 0),
    plotOutput("chi_dens"),
    plotOutput("chi_rep")
  ),
  tabPanel(
    title = "Reparitie Student",
    sliderInput(inputId = "stud_nr", label = "Bound", min = 10, max = 50, value = 20),
    sliderInput(inputId = "stud_df", label = "Degree of freedom", min = 0, max = 10, value = 3, step=0.1),
    sliderInput(inputId = "stud_cent", label = "Center", min = 0, max = 25, value = 0),
    numericInput(inputId = "stud_a", label = "a = ", value = 10, min = 0),
    numericInput(inputId = "stud_b", label = "b = ", value = 18, min = 0),
    plotOutput("stud_dens"),
    plotOutput("stud_rep")
  ),
  tabPanel(
    title = "Reparitie Log Normala",
    sliderInput(inputId = "lognorm_nr", label = "Bound", min = 10, max = 200, value = 100),
    sliderInput(inputId = "lognorm_medie", label = "Medie", min = 0, max = 200, value = 50),
    sliderInput(inputId = "lognorm_var", label = "Varianta", min = 1, max = 25, value = 10, step=0.25),
    numericInput(inputId = "lognorm_a", label = "a = ", value = 55, min = 0),
    numericInput(inputId = "lognorm_b", label = "b = ", value = 75, min = 0),
    plotOutput("lognorm_dens"),
    plotOutput("lognorm_rep")
  ),
  tabPanel(
    title = "Reparitie Normala",
    sliderInput(inputId = "norm_nr", label = "Bound", min = 10, max = 200, value = 100),
    sliderInput(inputId = "norm_medie", label = "Medie", min = -100, max = 100, value = 50),
    sliderInput(inputId = "norm_var", label = "Varianta", min = 1, max = 100, value = 10),
    numericInput(inputId = "norm_a", label = "a = ", value = 55, min = -100),
    numericInput(inputId = "norm_b", label = "b = ", value = 75, min = 0),
    plotOutput("norm_dens"),
    plotOutput("norm_rep")
  ),
  tabPanel(
    title = "Repartitie Cauchy",
    sliderInput(inputId ="loc_cauchy", label = "Alege punctul", min = -10, max = 10, value = 0),
    sliderInput(inputId ="scale_cauchy", label = "Alege scala", min = 0, max = 10, value =   5),
    sliderInput(inputId = "nr_cauchy", label = "Alege nr de experimente",min = 20, max = 100, value = 20),
    numericInput(inputId = "a_cauchy", label = "a = ", value = 5, min = 0),
    numericInput(inputId = "b_cauchy", label = "b = ", value = 5, min = 0),
    plotOutput("dens_cauchy"),
    plotOutput("rep_cauchy")
  ),
  tabPanel(
    title = "Repartitie Fisher",
    sliderInput(inputId ="gr1_fisher", label = "Alege gradul de libertate 1", min = 0, max = 50, value = 25),
    sliderInput(inputId ="gr2_fisher", label = "Alege gradul de libertate 2", min = 0, max = 50, value = 25),
    numericInput(inputId = "a_fisher", label = "a = ", value = 25, min = 0),
    numericInput(inputId = "b_fisher", label = "b = ", value = 30, min = 0),
    plotOutput("dens_fisher"),
    plotOutput("rep_fisher")
  ),
  tabPanel(
    title = "Repartitie Weibull",
    sliderInput(inputId ="shape_weibull", label = "Alege forma", min = 0.1, max = 10, value = 1.1),
    sliderInput(inputId ="scale_weibull", label = "Alege scala", min = 0.1, max = 10, value = 8.1),
    numericInput(inputId = "a_weibull", label = "a = ", value = 2, min = 0),
    numericInput(inputId = "b_weibull", label = "b = ", value = 5, min = 0),
    plotOutput("dens_weibull"),
    plotOutput("rep_weibull")
  )
)
)

plot_poligon <- function(x, a, b, fun){
  polygon(c(0, x[x<=a],a), 
          c(0, fun[x<=a],0), density = 15, angle = 135 , lty = 5, lwd = 2,
          col="black")
  polygon(c(a, x[x>=a & x<=b],b), 
          c(0, fun[x>=a & x<=b],0), density = 15, lty = 2, lwd = 2,
          col="black")
  polygon(c(b, x[x>=b], x[length(x)]), 
          c(0, fun[x>=b],0), density = 15, angle = 135, lwd = 2,
          col="black")
}

plot_continua <- function(x, fun, xlab, ylab, main){
  plot(x, fun, type = "l", lty=2, lwd=2,
       xlab=xlab, ylab=ylab, main=main)
}

plot_bars <- function(x, fun, a, b, l){
  barplot(names.arg = x, height = fun, xlab = "x", ylab = "Fx(x)", 
          main = "P(x<=a) P(a<=x<=b) P(x>=b)", 
          col = c(rep("blue", a+1), 
                  rep("gray", b - a),
                  rep("purple", l) ) )
}

server <- function(input, output, session) {
  #binomiala discreta
  output$masa_binomiala <- renderPlot({
    x <- seq(0, input$nr_expbin)
    plot(dbinom(x, input$nr_expbin, input$probbin), type = "h", xlab = "k", ylab = "P(X=k)", main = "Functia de masa")
  })
  output$rep_binomiala <- renderPlot({
    x <- seq(0, input$nr_expbin)
    plot(x, pbinom(x, input$nr_expbin, input$probbin), type = "s", xlab = "x", ylab = "Fx(x)", main = "Functia de repartitie")
  })
  output$probabilitati1bin <- renderPlot({
    x <- seq(0, input$nr_expbin)
    plot_bars(x,pbinom(x, input$nr_expbin, input$probbin), input$a_bin, input$b_bin,input$nr_expbin)
  })
  
  #hypergeometrica discreta
  
  output$masa_hipergeometrica <- renderPlot({
    x <- seq(0, input$khip)
    plot(x, dhyper(x, input$albehip, input$negrehip, input$khip), type = "h", xlab = "k", ylab = "P(X=k)", main = "Functia de masa")
  })
  
  output$rep_hipergeometrica <- renderPlot({
    x <- seq(0, input$khip)
    plot(x, phyper(x, input$albehip, input$negrehip, input$khip), type = "s", xlab = "x", ylab = "Fx(x)", main = "Functia de repartitie")
  })
  
  output$probabilitati1hip <- renderPlot({
    x <- seq(0, input$khip)
    plot_bars(x,phyper(x, input$albehip, input$negrehip, input$khip),input$a_hip,input$b_hip,input$khip)
  })
  
  
  #poisson discreta
  
  output$masa_poisson <- renderPlot({
    x <- seq(0, input$lambda * 5)
    plot(x, dpois(x,input$lambda), type = "h", xlab = "k", ylab = "P(X=k)", main = "Functia de masa a repartitiei Poisson")
  })
  output$rep_poisson <- renderPlot({
    x <- seq(0, input$lambda * 5)
    plot(x, ppois(x, input$lambda), type = "s", xlab = "x", ylab = "Fx(x)", main = "Functia de repartitie")
  })
  output$P1poisson <- renderPlot({
    x <- seq(0, input$lambda * 5)
    plot_bars(x,ppois(x, input$lambda),input$a_pois,input$b_pois,input$lambda * 5)
  })
  
  #geometrica discreta
  
  output$masa_geometrica <- renderPlot({
    x <- seq(0, 20)
    plot(x, dgeom(x, input$probgeom), type = "h", xlab = "k", ylab = "P(X=k)", main = "Functia de masa a repartitiei geometrice")
  })
  output$rep_geometrica <- renderPlot({
    x <- seq(0, 20)
    plot(x, pgeom(x, input$probgeom), type = "s", xlab = "x", ylab = "Fx(x)", main = "Functia de repartitie")
  })
  output$probabilitati1geom <- renderPlot({
    x <- seq(0, 20)
    plot_bars(x,pgeom(x, input$probgeom),input$a_geom,input$b_geom,20)
  })
  
  #uniforma discreta
  
  output$masa_duniforma <- renderPlot({
    x <- seq(0, input$max_dunif + input$min_dunif)
    plot(x, ddunif(x, input$min_dunif, input$max_dunif), type = "h", xlab = "k", ylab = "P(X=k)", main = "Functia de masa a repartitiei uniforme discrete")
  })
  output$rep_duniforma <- renderPlot({
    x <- seq(0, input$max_dunif + input$min_dunif)
    plot(x, pdunif(x, input$min_dunif, input$max_dunif), type = "s", xlab = "x", ylab = "Fx(x)", main = "Functia de repartitie")
  })
  output$probabilitati1dunif <- renderPlot({
    x <- seq(0, input$max_dunif + input$min_dunif)
    plot_bars(x,pdunif(x, input$min_dunif, input$max_dunif),input$a_dunif,input$b_dunif,input$max_dunif + input$min_dunif)
  })
  
  
  #exponentiala continua
  
  output$masa_exponentiala <- renderPlot({
    x <- seq(0, input$nr_expexp)
    dat <- data.frame(x=x, fx=dexp(x, rate=input$rata))
    ggplot(dat, aes(x=x, y=fx)) + geom_line() + ggtitle("Densitatea")
  })
  output$probabilitati1exp <- renderPlot({
    x <- seq(0, input$nr_expexp)
    plot(x, pexp(x, rate=input$rata), xlab = "x", ylab = "Fx(x)", main = "P(x<=a, a<=x<=b, x>=b)")
    plot_poligon(x = x,
                 a = input$a_exp,
                 b = input$b_exp,
                 fun = pexp(x, rate=input$rata))
  })
  
  #beta continua
  
  output$densitatea_beta <- renderPlot({
    x <- seq(0, 1,by = 0.02)
    plot(x,dbeta(x, input$shape1_beta, input$shape2_beta), type = "b", xlab = "k", ylab = "fx(x)",  main = "Densitatea repartitiei beta")
  })
  output$probabilitati1beta <- renderPlot({
    x <- seq(0, 1,by = 0.02)
    plot(x,pbeta(x, input$shape1_beta, input$shape2_beta), xlab = "x", ylab = "Fx(x)", main = "P(x<=a) P(a<=x<=b) P(x>=b)")
    plot_poligon(x = x,
                 a = input$a_beta,
                 b = input$b_beta,
                 fun = pbeta(x, input$shape1_beta, input$shape2_beta))  
  })
  
  #gamma continua
  output$densitatea_gamma <- renderPlot({
    x <- seq(0, 20)
    plot(x,dgamma(x, input$shape_gamma, input$rata_gamma), type = "b", xlab = "k", ylab = "fx(x)", main = "Densitatea repartitiei gamma")
  })
  output$probabilitati1gamma <- renderPlot({
    x <- seq(0, 20)
    fun <- pgamma(x, input$shape_gamma, input$rata_gamma)
    plot(x,fun, xlab = "x", ylab = "Fx(x)", main = "P(x<=a) P(a<=x<=b) P(x>=b)")
    plot_poligon(x = x,
                 a = input$a_gamma,
                 b = input$b_gamma,
                 fun = fun)
  })
  
  #uniforma continua
  output$dens_uniforma <- renderPlot({
    x <- seq(0,input$max_unif + input$min_unif, by = 0.1)
    plot(x, dunif(x, input$min_unif, input$max_unif), type = "l", xlab ="x", ylab = "fx(x)", main = "Densitatea repartitiei uniforme continue")
  })
  output$rep_uniforma <- renderPlot({
    x <- seq(0,input$max_unif + input$min_unif, by = 0.1 )
    plot(x, punif(x, input$min_unif, input$max_unif), type = "l", xlab = "x", ylab = "Fx(x)", main = "Functia de repartitie uniforma continua && P(x<=a) P(a<=x<=b) P(x>=b)")
    plot_poligon(x, input$a_unif, input$b_unif, punif(x, input$min_unif, input$max_unif))
  })
  
  #chi-squared continua
  output$chi_dens <- renderPlot({
    x <- seq(0, input$chi_nr, by=0.01)
    plot(x, dchisq(x, df=input$chi_df, ncp=input$chi_cent), type="l", xlab = "X", ylab = "fx(x)", main = "Densitate Chi square")
  })
  output$chi_rep <- renderPlot({
    x <- seq(0, input$chi_nr, by=0.01)
    plot(x, pchisq(x, df=input$chi_df, ncp=input$chi_cent), 
         type="l", xlab = "X", ylab = "Fx(x)", main = "Functia de repartitie Chi-Squared && P(x<=a) P(a<=x<=b) P(x>=b)")
    plot_poligon(x = x, a = input$chi_a, b = input$chi_b, fun = pchisq(x, df=input$chi_df, ncp=input$chi_cent))
  })
  
  #student 't' continua
  output$stud_dens <- renderPlot({
    #x <- seq(input$stud_nr / -2, input$stud_nr, by=0.1)
    #plot(x, dt(x, df=input$stud_df, ncp=input$stud_cent), type="l")
    plot(function(x) dt(x, df=input$stud_df, ncp=input$stud_cent), input$stud_nr / -2, input$stud_nr,
         xlab = "X", ylab = "fx(x)", main = "Densitatea repartitiei student")
  })
  output$stud_rep <- renderPlot({
    plot(function(x) pt(x, df=input$stud_df, ncp=input$stud_cent), input$stud_nr / -2, input$stud_nr,
         xlab = "X", ylab = "Fx(x)", main = "Functia de repartitie student && P(x<=a) P(a<=x<=b) P(x>=b)")
    x <- seq(input$stud_nr / -2, input$stud_nr, by=0.1)
    plot_poligon(x = x, a = input$stud_a, b = input$stud_b, fun = pt(x, df=input$stud_df, ncp=input$stud_cent))
  })
  
  #log normala continua
  output$lognorm_dens <- renderPlot({
    x <- seq(0.001, input$lognorm_nr, by=0.01)
    fun <- dlnorm(x, input$lognorm_medie, input$lognorm_var)
    plot_continua(x, fun, xlab = "X", ylab = "fx(x)", main = "Densitate repartitiei log normala")
  })
  output$lognorm_rep <- renderPlot({
    x <- seq(0.001, input$lognorm_nr, by=0.01)
    fun <- plnorm(x, mean = input$lognorm_medie, sd = input$lognorm_var)
    plot_continua(x, fun, xlab = "X", ylab = "Fx(x)", main = "Functia de repartitie log normala && P(x<=a) P(a<=x<=b) P(x>=b)")
    plot_poligon(x = x, a = input$lognorm_a, b = input$lognorm_b, fun = fun)
  })
  
  #normala continua
  output$norm_dens <- renderPlot({
    x <- seq(-input$norm_nr, input$norm_nr)
    plot(x, dnorm(x, mean = input$norm_medie, sd = input$norm_var), type = "l", lty=2, lwd=2,
         xlab = "X", ylab = "fx(x)", main = "Densitate repartitiei normale")
  })
  output$norm_rep <- renderPlot({
    x <- seq(-input$norm_nr, input$norm_nr)
    fun <- pnorm(x, mean = input$norm_medie, sd = input$norm_var)
    plot(x, fun, type = "l", lty=2, lwd=2, xlab = "X", ylab = "Fx(x)", main = "Functia de repartitie normala && P(x<=a) P(a<=x<=b) P(x>=b)")
    plot_poligon(x = x,
                 a = input$norm_a,
                 b = input$norm_b,
                 fun = fun)
  })
  
  #cauchy continua
  output$dens_cauchy <- renderPlot({
    x <- seq(-1*input$nr_cauchy,input$nr_cauchy, by = 0.1)
    plot(x, dcauchy(x, input$loc_cauchy, input$scale_cauchy), type = "l", xlab ="x", ylab = "f(x)", main = "Densitatea repartitiei Cauchy")
  })
  output$rep_cauchy <- renderPlot({
    x <- seq(-1*input$nr_cauchy, input$nr_cauchy, by = 0.1)
    plot(x, pcauchy(x, input$loc_cauchy, input$scale_cauchy), type = "l", xlab = "x", ylab = "F(x)", main = "Functia de repartitie Cauchy && P(x<=a) P(a<=x<=b) P(x>=b)")
    plot_poligon(x, input$a_cauchy, input$b_cauchy, pcauchy(x, input$loc_cauchy, input$scale_cauchy))
  })
  
  #fisher continua
  output$dens_fisher <- renderPlot({
    x <- seq(-1*(input$gr1_fisher + input$gr2_fisher),input$gr1_fisher + input$gr2_fisher, by = 0.1)
    plot(x, df(x, input$gr1_fisher, input$gr2_fisher), type = "l", xlab ="x", ylab = "f(x)", main = "Densitatea repartitiei Fisher")
  })
  output$rep_fisher <- renderPlot({
    x <- seq(-1*(input$gr1_fisher + input$gr2_fisher), input$gr1_fisher + input$gr2_fisher, by = 0.1)
    plot(x, pf(x, input$gr1_fisher, input$gr2_fisher), type = "l", xlab = "x", ylab = "F(x)", main = "Functia de repartitie Fisher && P(x<=a) P(a<=x<=b) P(x>=b)")
    plot_poligon(x, input$a_fisher, input$b_fisher, pf(x, input$gr1_fisher, input$gr2_fisher))
  })
  
  #weibull continua
  output$dens_weibull <- renderPlot({
    x <- seq(0,input$shape_weibull + input$scale_weibull, by = 0.01)
    plot(x, dweibull(x, input$shape_weibull, input$scale_weibull), type = "l", xlab ="x", ylab = "f(x)", main = "Densitatea repartitiei Weibull")
  })
  output$rep_weibull <- renderPlot({
    x <- seq(0, input$shape_weibull + input$scale_weibull, by = 0.1)
    plot(x, pweibull(x, input$shape_weibull, input$scale_weibull), type = "l", xlab = "x", ylab = "F(x)", main = "Functia de repartitie Weibull && P(x<=a) P(a<=x<=b) P(x>=b)")
    plot_poligon(x, input$a_weibull, input$b_weibull, pweibull(x, input$shape_weibull, input$scale_weibull))
  })
}

shinyApp(ui, server)