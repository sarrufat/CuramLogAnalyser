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
import vaadin.scala.MenuBar
import vaadin.scala.MenuBar.MenuItem
import vaadin.scala.Window
import vaadin.scala.TwinColSelect
import vaadin.scala.UI
import vaadin.scala.Button
import java.util.HashSet
import org.sarrufat.curam.log.analizer.SqlStatement
import vaadin.scala.SelectionMode
import vaadin.scala.ComboBox

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
    val sqldata = new TraceCollector(source).collectSQL()
    var currentFilteredTable: Seq[String] = Seq()
    val tab = new Table {
      sizeFull
      doFilters()
      sortable = false
      selectionMode = SelectionMode.Multi
      def doFilters() = {
        container = new BeanItemContainer(sqldata.filter { s ⇒ currentFilteredTable.find { _ == s.table } == None })
        visibleColumns = Seq("seq", "date", "stype", "table", "numberrows", "wherecond")
        columnHeaders = Seq("Line", "Date", "Statement", "Table", "#Rows", "Condition")
        itemDescriptionGenerator = { itemDesc ⇒
          itemDesc.propertyId match {
            case "wherecond" ⇒ itemDesc.itemId.asInstanceOf[SqlStatement].wherecond
            case _           ⇒ itemDesc.itemId.asInstanceOf[Statement].statement
          }
        }
        styleNames += "wordwrap-table"
        setColumnWidth("wherecond", 300)
      }
      var selectedItems: List[SqlStatement] = List()
      def selectType(typ: String, currentSelectedTables: Seq[String]) = {
        println(currentSelectedTables)
        unSelectType()
        selectedItems = TraceCollector.getByType(sqldata, typ).filter { s ⇒ if (currentSelectedTables.size == 0) true else currentSelectedTables.exists { t ⇒ t == s.table } }
        selectedItems.foreach { select(_) }
        if (selectedItems.size > 0) currentPageFirstItemId = selectedItems.head
      }
      def unSelectType() = { selectedItems.foreach { unselect(_) } }
    }
    components += menuBar()
    components += tab
    components -= dropBox
    def menuBar() = {
      def selectTableFilter() = {
        val window = new Window {
          caption = "Table selection"
          width = 600 px;
          val thisWindow = this
          import collection.JavaConversions._
          content = new VerticalLayout {
            components += new TwinColSelect {
              caption = "Select tables to exclude from view"
              width = 100 percent;
              TraceCollector.getTableList(sqldata).foreach { addItem(_) }
              val selected = new HashSet[String](currentFilteredTable)
              value = selected
              valueChangeListeners += { event ⇒
                //                println(event.property.value)
                currentFilteredTable = Seq()
                event.property.value.foreach { x ⇒
                  val set = x.asInstanceOf[java.util.Set[String]]
                  currentFilteredTable = set.toSeq
                }
                tab.doFilters()
              }
              immediate = true
            }
            components += Button("Select", click ⇒ { UI.current.windows -= thisWindow })
          }

        }
        window.center()
        UI.current.windows += window
      }
      var currentSelectedTables: Seq[String] = Seq()
      def selectType() = {
        val window = new Window {
          caption = "Search type"
          width = 600 px;
          val thisWindow = this
          import collection.JavaConversions._
          content = new VerticalLayout {
            val cb = new ComboBox {
              inputPrompt = "Search type";
              width = 100 percent;
              TraceCollector.SQLINSTRUCTIONS.foreach { addItem(_) }
            }
            components += cb
            components += new TwinColSelect {
              caption = "Select tables to exclude from view"
              width = 100 percent;
              TraceCollector.getTableList(sqldata).foreach { addItem(_) }
              val selected = new HashSet[String](currentSelectedTables)
              value = selected
              valueChangeListeners += { event ⇒
                //                println(event.property.value)
                currentSelectedTables = Seq()
                event.property.value.foreach { x ⇒
                  val set = x.asInstanceOf[java.util.Set[String]]
                  currentSelectedTables = set.toSeq
                }
              }
              immediate = true
            }

            components += Button("Search", click ⇒ {
              UI.current.windows -= thisWindow
              cb.value match {
                case Some(v) ⇒ tab.selectType(v.asInstanceOf[String], currentSelectedTables)
                case None    ⇒ tab.unSelectType()
              }
            })
          }
        }
        window.center()
        UI.current.windows += window
      }
      val mbar = new MenuBar {
        width = 300 px
      }
      val item1 = mbar.addItem("Filters")
      item1.addItem("Exclude tables", item ⇒ selectTableFilter())
      val item2 = mbar.addItem("Selection")
      item2.addItem("Sentence Type", item ⇒ selectType())
      mbar
    }
  }

}
