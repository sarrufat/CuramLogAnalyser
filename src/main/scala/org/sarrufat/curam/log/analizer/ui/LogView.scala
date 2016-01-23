package org.sarrufat.curam.log.analizer.ui

import com.vaadin.event.dd.DragAndDropEvent
import com.vaadin.event.dd.DropHandler
import com.vaadin.event.dd.acceptcriteria.AcceptAll
import com.vaadin.event.dd.acceptcriteria.AcceptCriterion
import com.vaadin.ui.DragAndDropWrapper
import vaadin.scala.Label
import vaadin.scala.Notification
import vaadin.scala.VerticalLayout
import vaadin.scala.intToMeasureOption
import com.vaadin.ui.Component
import com.vaadin.server.StreamVariable
import java.io.OutputStream
import java.io.ByteArrayOutputStream
import com.vaadin.server.StreamVariable.StreamingProgressEvent
import com.vaadin.server.StreamVariable.StreamingStartEvent
import com.vaadin.server.StreamVariable.StreamingEndEvent
import com.vaadin.server.StreamVariable.StreamingErrorEvent
import java.io.ByteArrayInputStream
import scala.io.Source
import vaadin.scala.Table
import vaadin.scala.BeanItemContainer
import org.sarrufat.curam.log.analizer.TraceCollector
import vaadin.scala.ScaladinWrapper
import vaadin.scala.mixins.ComponentMixin
import vaadin.scala.Alignment
import org.sarrufat.curam.log.analizer.Statement

class LogDropBox(val comp: Component, val showSql: (Source) ⇒ Unit) extends DragAndDropWrapper(comp) with DropHandler with ComponentMixin {
  class StreamV extends StreamVariable {
    val bas = new ByteArrayOutputStream
    def getOutputStream() = { bas }
    def listenProgress() = false
    def onProgress(ev: StreamingProgressEvent) = {}
    def streamingStarted(ev: StreamingStartEvent) = {}
    def streamingFinished(ev: StreamingEndEvent) = {
      val ba = bas.toByteArray()
      showSql(Source.fromBytes(ba))
    }
    def streamingFailed(ev: StreamingErrorEvent) = {}
    def isInterrupted() = false
  }
  setDropHandler(this)
  def drop(event: DragAndDropEvent): Unit = {
    val tr = event.getTransferable.asInstanceOf[WrapperTransferable]
    val filesOp = Option(tr.getFiles)
    filesOp.foreach { files ⇒
      for (file ← files) {
        val fname = file.getFileName
        if (!fname.endsWith(".log")) Notification.show("File rejected. You have to drag a log file.", Notification.Type.Warning)
        else if (file.getFileSize > LogView.FILE_SIZE_LIMIT) Notification.show(s"File rejected. Max ${LogView.FILE_SIZE_LIMIT / (1024 * 1024)}Mb files are accepted ", Notification.Type.Warning)
        else file.setStreamVariable(new StreamV)
      }
    }
  }
  def getAcceptCriterion(): AcceptCriterion = { AcceptAll.get }
}

object LogView {
  val FILE_SIZE_LIMIT = 100 * 1024 * 1024
}
class LogView extends VerticalLayout {
  spacing = false
  val dropPane = new VerticalLayout {
    add(new Label { value = "Drag the log file from desktop application or from the file system" })
    width = 300 px;
    height = 100 px;
    styleNames += "drop-area"
  }
  val dropBox = ScaladinWrapper.wrapComponent(new LogDropBox(dropPane.p, showSql))
  //  dropBox.setSizeUndefined
  //  ScaladinWrapper.wrapComponent(dropBox)
  components += dropBox
  setAlignment(dropBox, Alignment.TopLeft)
  def showSql(source: Source): Unit = {
    val tab = new Table {
      sizeFull
      container = new BeanItemContainer(new TraceCollector(source).collectSQL())
      visibleColumns = Seq("seq", "date", "stype", "table")
      itemDescriptionGenerator = { itemDesc ⇒
        itemDesc.itemId.asInstanceOf[Statement].statement
      }
    }
    components += tab
    setAlignment(dropBox, Alignment.TopCenter)

  }
}
