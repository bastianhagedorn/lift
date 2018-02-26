package utils.paternoster.gui

import javafx.scene.canvas.GraphicsContext
import javafx.scene.paint.Color
import javafx.scene.text.{Font, Text}

import lift.arithmetic.?
import utils.paternoster.logic.Graphics._
import utils.paternoster.logic.Scene.GridArrayNode

/**
  * Created by Federico on 18-Aug-17.
  */
object JavaFXRenderer {
  case class Context(gc:GraphicsContext, unitX:Double, unitY:Double, smallX:Double, smallY:Double, numberFont :Font , expressionFont: Font , width:Double,height:Double)

  def drawPrimitives(primitives:Iterable[GraphicalPrimitive], ctx:Context):Unit ={
    var newContext = adjustScaling(primitives,ctx)
    newContext = adjustCanvas(primitives,newContext)
    ctx.gc.clearRect(0, 0, newContext.width, newContext.height)
    ctx.gc.setFill(Color.WHITE)
    ctx.gc.fillRect(0,0,newContext.width,newContext.height)
    primitives.foreach(drawPrimitive(_, newContext))
  }

  def adjustScaling(primitives:Iterable[GraphicalPrimitive], ctx:Context):Context={
    val defaultYScaling = 60;
    var maxScaledWidth=0d
    var maxwidth= 0d
    var minheight= 1000d
    var maxheight = 0d
    var minScaledHeight = 1000d
    var maxScaledHeight = 0d
    val MAXSCALING = 3
    val MINSCALING = 1

    primitives.foreach(primitive =>primitive match {
      case BoxWithText(text, bx, by, bwidth, bheight) => {
        val currentScaledWidth = bwidth * ctx.unitX - 2 * ctx.smallX
        if ((currentScaledWidth) > maxScaledWidth) {
          maxScaledWidth = currentScaledWidth
          maxwidth = bwidth
        }
        val currentScaledHeight = bheight * ctx.unitY - 2 * ctx.smallY
        if ((currentScaledHeight) < minScaledHeight) {
          minScaledHeight = currentScaledHeight
          minheight = bheight
        }
        if ((currentScaledHeight) > maxScaledHeight) {
          maxScaledHeight = currentScaledHeight
          maxheight = bheight
        }
      }
      case Box(x, y, w, h) => {
        val currentScaledWidth = w * ctx.unitX - 2 * ctx.smallX
        if ((currentScaledWidth) > maxScaledWidth) {
          maxScaledWidth = currentScaledWidth
          maxwidth = w
        }
        val currentScaledHeight = h * ctx.unitY - 2 * ctx.smallY
        if ((currentScaledHeight) < minScaledHeight) {
          minScaledHeight = currentScaledHeight
          minheight = h
        }
        if ((currentScaledHeight) > maxScaledHeight) {
          maxScaledHeight = currentScaledHeight
          maxheight = h
        }
      }
      case CorneredClause(x, y, w, h) => {
        val currentScaledWidth = w * ctx.unitX - 2 * ctx.smallX
        if ((currentScaledWidth) > maxScaledWidth) {
          maxScaledWidth = currentScaledWidth
          maxwidth = w
        }
        val currentScaledHeight = h * ctx.unitY - 2 * ctx.smallY
        if ((currentScaledHeight) < minScaledHeight) {
          minScaledHeight = currentScaledHeight
          minheight = h
        }
        if ((currentScaledHeight) > maxScaledHeight) {
          maxScaledHeight = currentScaledHeight
          maxheight = h
        }
      }

      case _:Any =>
    })

      val newXScaling = Math.round(Math.max(MINSCALING,Math.min((ctx.width-2*ctx.smallX)/maxwidth, MAXSCALING)))
      var newYScaling = Math.round(Math.max(MINSCALING,newXScaling*0.5))

      Context(ctx.gc, 6, 6, ctx.smallX, ctx.smallY,ctx.numberFont,ctx.expressionFont,ctx.width,ctx.height)

  }

  def adjustCanvas(primitives:Iterable[GraphicalPrimitive],ctx:Context): Context ={
      var maxWidth=0.0
      var maxHeight=0.0
      primitives.foreach(primitive=> primitive match {
      case BoxWithText(_,x,y,w,h) => {
        val width = x+w
        val height = y+h
        if(width > maxWidth) maxWidth=width
        if(height > maxHeight) maxHeight = height
      }
      case Box(x,y,w,h)=>  {
        val width = x+w
        val height = y+h
        if(width > maxWidth) maxWidth=width
        if(height > maxHeight) maxHeight = height
      }
      case CorneredClause(x,y,w,h)=>  {
        val width = x+w
        val height = y+h
        if(width > maxWidth) maxWidth=width
        if(height > maxHeight) maxHeight = height
      }
      case Rectangle(x,y,w,h)=> {
        val width = x+w
        val height = y+h
        if(width > maxWidth) maxWidth=width
        if(height > maxHeight) maxHeight = height
      }
      case default =>
    })

    if(maxHeight*ctx.unitY > TypeVisualizer.getMainPane().height){
      TypeVisualizer.getMainPane().setCanvasHeight(maxHeight*ctx.unitY)
    }
    if(maxWidth*ctx.unitX > TypeVisualizer.getMainPane().width){
      TypeVisualizer.getMainPane().setCanvasWidth(maxWidth*ctx.unitX)
    }
    Context(TypeVisualizer.getMainPane().getGraphicsContext(), ctx.unitX, ctx.unitY, ctx.smallX, ctx.smallY,ctx.numberFont,ctx.expressionFont,TypeVisualizer.getMainPane().canvas.getWidth,TypeVisualizer.getMainPane().canvas.getHeight)
  }

  def drawPrimitive(primitive:GraphicalPrimitive, ctx: Context) = {

    primitive match {
      case DashedBox(x,y,w,h)=>
        ctx.gc.setLineDashes(2)
        ctx.gc.setLineDashOffset(1)
        ctx.gc.setFont(ctx.numberFont)
        ctx.gc.setFill(Color.GREY)
        ctx.gc.strokeRect(
          x*ctx.unitX + ctx.smallX,
          y*ctx.unitY + ctx.smallY,
          w*ctx.unitX - 2*ctx.smallX,
          h*ctx.unitY - 2*ctx.smallY
        )

      case ExpressionSource(text,begin,end, x,y)=>
        ctx.gc.setLineDashes()
        ctx.gc.setFont(ctx.expressionFont)

        var beforeHightLightText = text.substring(0,begin)
        var highLightText = text.substring(begin, end+1)
        var afterHighLightText = text.substring(end+1,text.length)

        var beforeHightLight = new Text(beforeHightLightText)
        var highLight = new Text(highLightText)
        var afterHighLight = new Text(afterHighLightText)
        beforeHightLight.setFont(ctx.expressionFont)
        highLight.setFont(ctx.expressionFont)
        afterHighLight.setFont(ctx.expressionFont)

        var beforeLength = beforeHightLight.getLayoutBounds.getWidth
        var highLightLength = highLight.getLayoutBounds.getWidth
        var afterLength = afterHighLight.getLayoutBounds.getWidth

        //Paint text before highlighting
        ctx.gc.strokeText(beforeHightLightText,
          x,
          y*ctx.unitY + ctx.smallY)

        //Paint highlighted text
        ctx.gc.setStroke(Color.RED)
        ctx.gc.strokeText(highLightText,
          (x+beforeLength),
          y*ctx.unitY + ctx.smallY)

        //Paint text after highlighting
        ctx.gc.setStroke(Color.BLACK)
        ctx.gc.strokeText(afterHighLightText,
          (x+beforeLength+highLightLength),
          y*ctx.unitY + ctx.smallY)

      case BoxWithText(text,bx,by,bwidth,bheight)=>
        ctx.gc.setLineDashes()
        ctx.gc.setFont(ctx.numberFont)
        ctx.gc.setFill(Color.BLACK)
        ctx.gc.strokeRect(
          bx*ctx.unitX + ctx.smallX,
          by*ctx.unitY + ctx.smallY,
          bwidth*ctx.unitX - 2*ctx.smallX,
          bheight*ctx.unitY - 2*ctx.smallY
        )

        val textX = ((bx*ctx.unitX + ctx.smallX)+(bwidth*ctx.unitX - 2*ctx.smallX))-(ctx.gc.getFont.getSize*text.size)/*Math.min((ctx.gc.getFont.getSize*text.size),(bwidth*ctx.unitX - 2*ctx.smallX)/2)*/
        val textY =  ((by*ctx.unitY + ctx.smallY)+(bheight*ctx.unitY - 2*ctx.smallY)) /*((by*ctx.unitY + ctx.smallY)+(bheight*ctx.unitY - 2*ctx.smallY))-((bheight*ctx.unitY - 2*ctx.smallY)*0.025)*/
        //ctx.gc.setFont(new Font(ctx.gc.getFont.getName,10))
        ctx.gc.strokeText(text,textX,textY)
      case Rectangle(x, y, w, h) =>
        ctx.gc.setLineDashes()
        ctx.gc.setFont(ctx.numberFont)
        ctx.gc.setFill(Color.DARKGREEN)
        ctx.gc.fillRect(
          Math.round(x*ctx.unitX + ctx.smallX*4),
          Math.round(y*ctx.unitY + ctx.smallY*4),
          Math.round(w*ctx.unitX - 2*ctx.smallX),
          Math.round(h*ctx.unitY - 2*ctx.smallY))
      case CorneredClause(x, y, w, h) =>
        ctx.gc.setLineDashes()
        ctx.gc.setFont(ctx.numberFont)
        ctx.gc.setFill(Color.BLACK)
        //Left line
        ctx.gc.strokeLine(
          x*ctx.unitX + ctx.smallX,
          y*ctx.unitY + ctx.smallY,
          x*ctx.unitX + ctx.smallX,
          (y*ctx.unitY + ctx.smallY)+(h*ctx.unitY - 2*ctx.smallY)
        )
        //Left upper corner
        ctx.gc.strokeLine(
          x*ctx.unitX + ctx.smallX,
          y*ctx.unitY + ctx.smallY,
          x*ctx.unitX + ctx.smallX+(2),
          y*ctx.unitY + ctx.smallY
        )
        //Left lower corner
        ctx.gc.strokeLine(
          x*ctx.unitX + ctx.smallX,
          (y*ctx.unitY + ctx.smallY)+(h*ctx.unitY - 2*ctx.smallY),
          x*ctx.unitX + ctx.smallX+(2),
          (y*ctx.unitY + ctx.smallY)+(h*ctx.unitY - 2*ctx.smallY)
        )

        //Right line
        ctx.gc.strokeLine(
          (x*ctx.unitX + ctx.smallX)+(w*ctx.unitX - 2*ctx.smallX),
          y*ctx.unitY + ctx.smallY,
          (x*ctx.unitX + ctx.smallX)+(w*ctx.unitX - 2*ctx.smallX),
          (y*ctx.unitY + ctx.smallY)+(h*ctx.unitY - 2*ctx.smallY)
        )
        //Right upper corner
        ctx.gc.strokeLine(
          (x*ctx.unitX + ctx.smallX)+(w*ctx.unitX - 2*ctx.smallX),
          y*ctx.unitY + ctx.smallY,
          (x*ctx.unitX + ctx.smallX)+(w*ctx.unitX - 2*ctx.smallX)-(2),
          y*ctx.unitY + ctx.smallY
        )
        //Right lower corner
        ctx.gc.strokeLine(
          (x*ctx.unitX + ctx.smallX)+(w*ctx.unitX - 2*ctx.smallX),
          (y*ctx.unitY + ctx.smallY)+(h*ctx.unitY - 2*ctx.smallY),
          (x*ctx.unitX + ctx.smallX)+(w*ctx.unitX - 2*ctx.smallX)-(2),
          (y*ctx.unitY + ctx.smallY)+(h*ctx.unitY - 2*ctx.smallY)
        )
      case Seperator(x,y)=>{
        ctx.gc.setLineDashes()
        ctx.gc.setFont(ctx.numberFont)
        ctx.gc.setFill(Color.BLACK)
        ctx.gc.strokeLine(
          x*ctx.unitX + ctx.smallX,
          y*ctx.unitY + ctx.smallY,
          x*ctx.unitX + ctx.smallX,
          y*ctx.unitY + ctx.smallY-(2))
      }
      case Box(x, y, w, h) =>
        ctx.gc.setLineDashes()
        ctx.gc.setFont(ctx.numberFont)
        ctx.gc.setStroke(Color.RED)
        ctx.gc.strokeRect(
          x*ctx.unitX + ctx.smallX,
          y*ctx.unitY + ctx.smallY,
          w*ctx.unitX - 2*ctx.smallX,
          h*ctx.unitY - 2*ctx.smallY)
    }
  }

  private def drawArrow(gc:GraphicsContext, node1X:Double, node1Y:Double, node2X:Double, node2Y:Double) {
    val arrowAngle = Math.toRadians(45.0)
    val arrowLength = 10.0
    val dx = node1X - node2X
    val dy = node1Y - node2Y
    val angle = Math.atan2(dy, dx)
    val x1 = Math.cos(angle + arrowAngle) * arrowLength + node2X
    val y1 = Math.sin(angle + arrowAngle) * arrowLength + node2Y

    val x2 = Math.cos(angle - arrowAngle) * arrowLength + node2X
    val y2 = Math.sin(angle - arrowAngle) * arrowLength + node2Y
    gc.strokeLine(node1X, node1Y, node2X, node2Y)
    gc.strokeLine(node2X, node2Y, x1, y1)
    gc.strokeLine(node2X, node2Y, x2, y2)
  }
}