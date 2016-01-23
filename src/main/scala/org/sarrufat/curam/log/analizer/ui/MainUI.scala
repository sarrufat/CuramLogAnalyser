package org.sarrufat.curam.log.analizer.ui

import javax.servlet.annotation.WebServlet
import vaadin.scala._
import vaadin.scala.server.ScaladinServlet

@WebServlet(urlPatterns = Array("/*"))
class Servlet extends ScaladinServlet(ui = classOf[MainUI])

class MainUI extends UI(theme = ValoTheme.ThemeName) {
  content = new VerticalLayout { layout ⇒
    margin = true
    spacing = true
    //    sizeFull

    setAlignment(addComponent(new Label {
      value = "Welcome to SQL Log analyzer for Cúram applications"
      styleNames += ValoTheme.LabelH1
      sizeUndefined
    }), Alignment.TopLeft)
    // Setup the layout that will contain the views
    setAlignment(addComponent(new HorizontalLayout {
      margin = false
      spacing = false
      add(new LogView)
    }), Alignment.TopCenter)
  }
}
