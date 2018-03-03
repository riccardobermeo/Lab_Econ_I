Funciones

para saber una función
help("function")

'raices cuadradas'
sqrt(x)
'regresar el n fila y n reglon una matriz'
nrox(A)
ncol(A)

creando funciones
'numero al cuadrado'
cua=function(x){
  y=x^2
  return(y)
}

raiz=function(x){
  if(x<0){
    print("error matematico")
  }
    else{
      return(x^.5)
    }
}
  
'hipotenusa'
hyp=function(x,y){
  z=((x^2)+y^2)^.5
  return(z)
}

chic=function(a,b,c){
  cond=b^2>=4*a*c
  if(cond){
    z1=((-b+(b^2-4*c*a)^.5)/2)
    z2=((-b-(b^2-4*c*a)^.5)/2)
    z=c(z1,z2)
    return(z)
  }
  else{
    print("el pol tiene raices complejas")
  }
}

'loops'
'se utilizan para repetir una secuencia sobre un algoritmo, son dos for y while'
x=1
while(x<5){
  x=x+1
  if(x==3) next
  print(x)
}

'ahora el for'
for(i in 1:30){
  print(paste("el loop",i))
  print(i*i)
}

'el algoritmo de euclides te regresa el maximo comun multiplo entre dos numeros enteros'
mcd <- function(a, b){
  A = a
  B = b
  if(A!=0){
    while(B != 0) {
      if(A>B){
        A=A-B
      } else {
        B=B-A
      } 
    }
    return(A)
  } else {
    return(B)
  }
}

