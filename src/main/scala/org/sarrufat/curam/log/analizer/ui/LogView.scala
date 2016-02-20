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
import vaadin.scala.HorizontalLayout
import vaadin.scala.server.FontAwesome
import scala.util.Try

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
      var totalSetter: Option[String ⇒ Unit] = None
      sizeFull
      doFilters()
      sortable = false
      selectionMode = SelectionMode.Multi
      styleNames += "small"
      width = 100 percent
      def doFilters() = {
        container = new BeanItemContainer(sqldata.filter { s ⇒ currentFilteredTable.find { cur ⇒ s.tables.exists { x ⇒ x == cur } } == None })
        visibleColumns = Seq("seq", "date", "stype", "stables", "numberrows", "wherecond")
        columnHeaders = Seq("Line", "Date", "Statement", "Tables", "#Rows", "Condition")
        itemDescriptionGenerator = { itemDesc ⇒
          itemDesc.propertyId match {
            case "wherecond" ⇒ itemDesc.itemId.asInstanceOf[SqlStatement].wherecond
            case _           ⇒ itemDesc.itemId.asInstanceOf[Statement].statement
          }
        }
        styleNames += "wordwrap-table"
        setColumnWidth("wherecond", 400)
      }
      var selectedItems: List[SqlStatement] = List()
      def selectType(typ: Option[String], currentSelectedTables: Seq[String]) = {
        println(currentSelectedTables)
        unSelectType()
        selectedItems = TraceCollector.getByType(sqldata, typ).filter { s ⇒ if (currentSelectedTables.size == 0) true else currentSelectedTables.exists { t ⇒ s.tables.exists { x ⇒ x == t } } }
        selectedItems.foreach { select(_) }
        if (selectedItems.size > 0) currentPageFirstItemId = selectedItems.head else currentPageFirstItemId = None
        totalSetter.foreach(_("Found: " + selectedItems.size))
      }
      def netxSelect() = {
        selectedItems match {
          case List() ⇒ currentPageFirstItemId = None
          case _ ⇒ currentPageFirstItemId.foreach { id ⇒
            Try(currentPageFirstItemId = selectedItems(selectedItems.indexOf(id) + 1)).getOrElse(currentPageFirstItemId = selectedItems.head)
          }
        }
      }
      def prevSelect() = {
        selectedItems match {
          case List() ⇒ currentPageFirstItemId = None
          case _ ⇒ currentPageFirstItemId.foreach { id ⇒
            Try(currentPageFirstItemId = selectedItems(selectedItems.indexOf(id) - 1)).getOrElse(currentPageFirstItemId = selectedItems.last)
          }
        }
      }
      def unSelectType() = { selectedItems.foreach { unselect(_) } }
    }
    components += menuBar();
    lazy val (c, f) = searchButtons(tab.netxSelect, tab.prevSelect)
    tab.totalSetter = Some(f)
    components += c
    components += tab
    components -= dropBox
      def searchButtons(next: ⇒ Unit, prev: ⇒ Unit) = {
        val lab = Label("")
        (new HorizontalLayout {
          spacing = true
          val left = Button(FontAwesome.ArrowLeft.html, prev)
          left.htmlContentAllowed = true
          components += left
          val right = Button(FontAwesome.ArrowRight.html, next)
          right.htmlContentAllowed = true
          components += right
          components += lab
        },
          { total: String ⇒ lab.caption = total })
      }
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
                  caption = "Select tables"
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
                    case Some(v) ⇒ tab.selectType(Option(v.asInstanceOf[String]), currentSelectedTables)
                    case None    ⇒ if (currentSelectedTables.isEmpty) tab.unSelectType() else tab.selectType(None, currentSelectedTables)
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
