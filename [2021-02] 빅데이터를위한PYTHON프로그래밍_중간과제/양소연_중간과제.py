import turtle as t
import random

####################################### 
# 게임 설정
####################################### 
score = 0
maxscore = 50
####################################### 
# 첫번째 : 화면 객체
#######################################
# 배경화면 설정
screen = t.Screen()
screen.setup(1000,600)
screen.bgpic('C:\\Users\\syeon\\Desktop\\game\\underwater.png')

# 낚시꾼 이미지 등록
fisher = "C:\\Users\\syeon\\Desktop\\game\\fisher.gif"
screen.register_shape(fisher)

# 물고기 이미지 등록
imgc = "C:\\Users\\syeon\\Desktop\\game\\3.gif"
screen.register_shape(imgc)
imge = "C:\\Users\\syeon\\Desktop\\game\\5.gif"
screen.register_shape(imge)
imgf = "C:\\Users\\syeon\\Desktop\\game\\6.gif"
screen.register_shape(imgf)
imgg = "C:\\Users\\syeon\\Desktop\\game\\7.gif"
screen.register_shape(imgg)
imgh = "C:\\Users\\syeon\\Desktop\\game\\8.gif"
screen.register_shape(imgh)
screen.listen( )

####################################### 
# 두번째 : 텍스트 객체
####################################### 
# 점수
texta = t.Turtle()
texta.penup()
texta.hideturtle()
texta.goto(0,250)
texta.color("white")
texta.write("점수:0점", False,"center",("굴림체",16))
####################################### 
# 세번째 : 거북이 객체
#######################################
# 물고기

ta = t.Turtle(shape=imgc)
ta.up()
ta.goto( -300, -100)

tb = t.Turtle(shape=imge)
tb.up()
tb.goto( -125, -10)

tc = t.Turtle(shape=imgf)
tc.up()
tc.goto( -350, -30)

td = t.Turtle(shape=imgg)
td.up()
td.goto( -150, -150)

te = t.Turtle(shape=imgh)
te.up()
te.goto( -350, -200)

 
# 낚시꾼
fisher = t.Turtle(shape=fisher)
fisher.up()
fisher.goto(430, -50)

# 낚시 동선그리기
fire = t.Turtle()
fire.shapesize(2,4,1)
fire.pencolor("yellow")
fire.color("yellow")
fire.up()
fire.goto( 300, 40 )
fire.setheading( 130 )
fire.down()

####################################### 
# 함수 정의
#######################################
# 1.키보드 이벤트 관련 함수 정의
def up():      # 위
    fire.setheading( fire.heading() + 2 )

def down():      # 아래
    fire.setheading( fire.heading() - 2 )


# 2.게임이 실행되는 메인 함수
 

def fires():
    global score, maxscore
    success = 0
    if 1==1:
        for i in range(    random.randint(40, 60) ):
            fire.forward(  random.randint(5,25) )
            fire.left( random.randint(0,5) )

        if fire.distance(ta, 0)<40:
            success = 1   # distance 731.7103251970686 / 740
        if fire.distance(tb, 0)<40:
            success = 1   # distance 556.4395744373328 / 475
        if fire.distance(tc, 0)<40:
            success = 1   # distance 780.2563681252464 / 720
        if fire.distance(td, 0)<40:
            success = 1   # distance 588.5575587824865 / 640
        if fire.distance(te, 0)<40:
            success = 1   # distance 794.2921376924236 / 890
        fire.clear()
        fire.down( )
        if success == 1 :
            score = score+10
            texta.clear()
            texta.write("점수:"+str(score)+"점",False,"center",("굴림체",16))
            fire.write("성공",False,"center",("굴림체",16))
        else:
            fire.write("실패"+str(fire.xcor())+str(fire.ycor()),False,"center",("굴림체",16))
        fire.up()   
        fire.goto( 300, 40 )
        fire.down()
        fire.setheading( 135 )

####################################### 
# 이벤트 정의 및 play( ) 함수 호출
####################################### 
# 키보드 클릭 이벤트
screen.onkeypress(up, "Up")
screen.onkeypress(down, "Down")
screen.onkeypress(fires, "space")
screen.listen()
