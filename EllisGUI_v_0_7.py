import sys, os, time, datetime

import numpy as np
import pandas as pd
import folium
# https://mapsplatform.google.com/pricing/?_gl=1*1s4atal*_ga*MTQwMzE0MzgxOC4xNjY3NDgyMjQx*_ga_NRWSTWS78N*MTY2NzQ4MjI0MS4xLjEuMTY2NzQ4MjI0Ni4wLjAuMA..
from PyQt5.QtWidgets import QMainWindow, QApplication, QWidget, QTabWidget, QVBoxLayout, QLabel, QGridLayout, QLineEdit, \
    QPushButton, QMessageBox, QTableView, QComboBox, QStyle, QSpacerItem, QSizePolicy
from PyQt5 import QtCore, QtWebEngineWidgets
from PyQt5.QtGui import QRegExpValidator
from PyQt5.QtCore import Qt, QSortFilterProxyModel, QSize, QRegExp

# Project #


# Creating the main window
class App(QMainWindow):
    def __init__(self):
        super().__init__()
        self.title = 'Ellis Land Surveying'
        self.left = 200
        self.top = 200
        self.width = 1300
        self.height = 800
        self.setGeometry(self.left, self.top, self.width, self.height)
        self.setWindowTitle(self.title)

        property_id = [12345, 9876]
        street_number = [0, 7820]
        street_name = ["Test Street", "Seawall Blvd"]
        city = ["La La Land", "Galveston"]
        zip = [98765, 77551]
        state = ["Murica", "Tx"]
        client_id = [99, 100]
        lat = [None, 29.2578624]
        long = [None, -94.84304617526199]
        description = [None, "Oceanfront Loft Apartments"]
        status = ["Finalized", "Field Work Complete"]
        invoiceDate = ["2022-11-01", None]
        lastChangedBy = ["kreed", "kreed"]
        notes = ["Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed", ""]
        propDF = pd.DataFrame({"property_id": property_id,
                               "street_number": street_number,
                               "street_name": street_name,
                               "city": city,
                               "zip": zip,
                               "state": state,
                               "lat": lat,
                               "long": long,
                               "description": description,
                               "status": status,
                               "notes": notes,
                               "client_id": client_id,
                               "invoice_date": invoiceDate,
                               "last_changed_by": lastChangedBy
                               })
        client_id = [99, 100]
        client_first_name = ["Jane", "Jack"]
        client_last_name = ["Joe", "Doe"]
        client_phone = ["xxx-xxx-xxxx", "864-789-4555"]
        client_phone_2 = [None, "yyy-yyy-yyyy"]
        client_email = ["na@na.com", None]
        client_address_number = [12345, 0]
        client_address_number_2 = [None, "#14"]
        client_address_street = ["road street", "NASA Pkwy"]
        client_address_city = ["Galveston", "Houston"]
        client_address_state = ["Tx", "Tx"]
        client_address_zip = [0000, 778888]

        clientDF = pd.DataFrame({"client_id": client_id,
                                 "client_first_name": client_first_name,
                                 "client_last_name": client_last_name,
                                 "client_phone": client_phone,
                                 "client_phone_2": client_phone_2,
                                 "client_email": client_email,
                                 "client_address_number": client_address_number,
                                 "client_address_2": client_address_number_2,
                                 "client_address_street": client_address_street,
                                 "client_address_city": client_address_city,
                                 "client_address_state": client_address_state,
                                 "client_address_zip": client_address_zip
                                 })
        propDF.sort_values('property_id', ascending=False, inplace=True)
        self.tabWidget = QTabWidget()
        self.clientManagementTab = ClientManagementTab(clientDF)
        self.newJobTab = NewJobTab(propDF, clientDF)
        self.jobManagementTab = JobManagementTab(propDF)
        self.jobViewTab = JobViewTab(propDF)

        self.tabWidget.addTab(self.newJobTab, "New Job")
        self.tabWidget.addTab(self.clientManagementTab, "Client Management")
        self.tabWidget.addTab(self.jobViewTab, "Job View")
        self.tabWidget.addTab(self.jobManagementTab, "Job Management")
        self.setCentralWidget(self.tabWidget)
        self.clientManagementTab.submitClient.clicked.connect(self.updateClientData)
        self.newJobTab.submitButton.clicked.connect(self.updatePropDataNewJob)
        self.jobManagementTab.view.model().dataChanged.connect(self.updatePropDataUpdateJob)

        self.show()

    def updateClientData(self):
        # print("Job Tab Client Update")
        self.newJobTab.clientDF = self.clientManagementTab.clientDF

    def updatePropDataNewJob(self):
        # self.newJobTab.submitData()
        self.jobViewTab.propDF = self.newJobTab.propDF
        self.jobViewTab.createMap()

        self.tabWidget.removeTab(3)
        newDF = self.newJobTab.propDF
        newDF.sort_values('property_id', ascending=False, inplace=True)
        newDF.reset_index(inplace=True, drop=True)
        self.jobManagementTab = JobManagementTab(newDF)
        self.tabWidget.addTab(self.jobManagementTab, "Job Management")
        self.jobManagementTab.view.model().dataChanged.connect(self.updatePropDataUpdateJob)

    def updatePropDataUpdateJob(self):
        # print("parent APP found the change")
        self.jobViewTab.propDF = self.jobManagementTab.propDF
        self.newJobTab.propDF = self.jobManagementTab.propDF
        self.jobViewTab.createMap()


# Creating tab widgets
class NewJobTab(QMainWindow):
    def __init__(self, propDF, clientDF):
        super(NewJobTab, self).__init__()
        self.clientSearch = None
        self.clientID = None
        self.updateData = None
        self.geocodeMessage = ""
        self.clientDF = clientDF
        self.propDF = propDF
        self.clientDF = clientDF

        container = QWidget()
        centralWidget = CentralWidget(container)
        self.setCentralWidget(centralWidget)
        self.layout = QGridLayout(container)

        startRowProp = 1

        self.propertyIDLab = QLabel()
        self.propertyIDLab.setText("PropertyID:")
        self.layout.addWidget(self.propertyIDLab, startRowProp, 0)

        self.propertyIDInput = QLineEdit()
        self.propertyIDInput.setMaximumWidth(200)
        self.layout.addWidget(self.propertyIDInput, startRowProp, 1)

        # Address Inputs
        self.addNumInput = QLineEdit()
        self.addNameInput = QLineEdit()
        self.addCityInput = QLineEdit()
        self.addZipInput = QLineEdit()
        self.addStateInput = QLineEdit()
        # Client Inputs
        self.clientFirstNameValue = QLabel()
        self.clientLastNameValue = QLabel()
        self.clientPhoneValue = QLabel()

        self.noteField = QLineEdit()

        # Validators
        # Validators
        numericRegex = QRegExp("[0-9].+")
        numericValidator = QRegExpValidator(numericRegex)
        zipValidator = QRegExp("[0-9]{5}")
        zipValidator = QRegExpValidator(zipValidator)
        self.addNumInput.setValidator(numericValidator)
        self.addNumInput.setValidator(numericValidator)
        #self.addZipInput.setInputMask("99999")
        self.addZipInput.setValidator(zipValidator)
        self.addStateInput.setInputMask("AA")
        #self.propertyIDInput.setValidator(numericValidator)

        # ----Buttons----
        self.propFindButton = QPushButton()
        self.propFindButton.setText("Search for Property ID")
        self.layout.addWidget(self.propFindButton, startRowProp, 2)
        self.propFindButton.clicked.connect(self.propExitsts)
        self.propertyIDInput.returnPressed.connect(self.propFindButton.click)
        self.submitButton = QPushButton()
        self.submitButton.clicked.connect(self.submitData)

    def propExitsts(self):
        print("searching for Prop")

        if self.addStateInput.visibleRegion().isEmpty():
            self.createFields()

        try:
            userInput = int(self.propertyIDInput.text())
        except:
            errorBox = QMessageBox()
            errorBox.setWindowTitle("Invalid Property ID")
            errorBox.setText("Property ID must contain only numbers")
            errorBox.setIcon(QMessageBox.Critical)
            e = errorBox.exec_()
            self.clearFields()

        if sum(self.propDF.property_id.astype('int') == userInput) > 0:
            foundPropBox = QMessageBox()
            foundPropBox.setWindowTitle("Property ID Exists!")
            foundPropBox.setText(
                "This Property ID has been found in your database already.\nWould you like to import this property information?\n\nNote: Declining this message and saving new data for this property ID will overwrite what is currently stored in the database")
            foundPropBox.setIcon(QMessageBox.Question)
            foundPropBox.setStandardButtons(QMessageBox.No | QMessageBox.Yes)
            foundPropResp = foundPropBox.exec_()  # 16384 = Yes , No = 65536
        else:
            foundPropResp = 0

        if foundPropResp == 16384:
            print("Importing Property Data")
            foundPropSeries = self.propDF.loc[self.propDF.property_id.astype('int') == int(userInput)]
            foundPropSeries = foundPropSeries.merge(self.clientDF, on="client_id")
            foundPropSeries = foundPropSeries.iloc[0]
            self.addNameInput.setText(foundPropSeries.street_name)
            self.addNumInput.setText(str(foundPropSeries.street_number))
            self.addCityInput.setText(foundPropSeries.city)
            self.addZipInput.setText(str(foundPropSeries.zip))
            self.addStateInput.setText(foundPropSeries.state)
            self.clientFirstNameValue.setText(foundPropSeries.client_first_name)
            self.clientLastNameValue.setText(foundPropSeries.client_last_name)
            self.clientPhoneValue.setText(str(foundPropSeries.client_phone))
            self.clientID = int(foundPropSeries.client_id)



        else:
            print("Doesn't Exist")
            noPropBox = QMessageBox()
            noPropBox.setWindowTitle("Property ID not in Database")
            noPropBox.setText("Could not Find this Property ID in the Database. Please Enter new Data")
            noPropBox.setIcon(QMessageBox.Information)
            noPropBox.setStandardButtons(QMessageBox.Ok)
            noPropBox.exec_()
            currentID = self.propertyIDInput.text()
            self.clearFields()
            self.propertyIDInput.setText(currentID)

    def findClient(self):
        print("Searching for Client")
        if self.clientSearch is None:
            self.clientSearch = ClientSearchWindow(self.clientDF)
        self.clientSearch.show()
        self.clientSearch.selectButton.clicked.connect(self.importClientParent)

    def submitData(self):
        if self.clientID is not None and self.addNameInput != "":
            try:
                self.updateData = pd.DataFrame({"property_id": int(self.propertyIDInput.text()),
                                                "street_number": int(self.addNumInput.text()),
                                                "street_name": self.addNameInput.text(),
                                                "city": self.addCityInput.text(),
                                                "zip": int(self.addZipInput.text()),
                                                "state": self.addStateInput.text(),
                                                "lat": None,
                                                "long": None,
                                                "description": None,
                                                "status": "Not Started",
                                                "notes": self.noteField.text(),
                                                "client_id": self.clientID,
                                                "invoice_date": None,
                                                "last_changed_by": os.getlogin()
                                                }, index=[0])
                #Geocode the property
                self.geocodeProp()
                if sum(self.propDF.property_id.astype('str') == str(self.propertyIDInput.text())) > 0:
                    print("Updating Existing Property Data...")
                    print(self.propDF.property_id == int(self.propertyIDInput.text()))
                    exInd = self.propDF[self.propDF.property_id == int(self.propertyIDInput.text())].index.item()
                    self.propDF.iloc[exInd] = self.updateData.iloc[0]
                    subMessage = "Existing Property Information has been Updated"
                else:
                    print("Submitting New Property Data...")
                    self.propDF = pd.concat([self.propDF, self.updateData])
                    self.propDF.reset_index(drop=True, inplace=True)
                    subMessage = "New Property Information has been Entered"
                self.clearFields()
            except:
                subMessage = "Failed to Submit Data. Please Ensure all Mandatory Fields are Filled In"
        else:
            subMessage = "Missing Data.\nPlease Ensure a client is associated with the property and all mandatory fields are filled in"
        subPropBox = QMessageBox()
        subPropBox.setWindowTitle("Submission Status")
        subPropBox.setText(subMessage + self.geocodeMessage)
        subPropBox.setStandardButtons(QMessageBox.Ok)
        subPropBox.exec_()

    def geocodeProp(self):
        geocodeDF = self.updateData.astype('str')
        geocodeDF[
            "urlString"] = geocodeDF.street_number + " " + geocodeDF.street_name + ", " + geocodeDF.city + ", " + geocodeDF.state
        address = geocodeDF.urlString[0]
        # Limit one response per second. No repeated hits https://operations.osmfoundation.org/policies/nominatim/
        # url = 'https://nominatim.openstreetmap.org/search/' + urllib.parse.quote(address) + '?format=json'
        # response = requests.get(url).json()
        lat = 29.2578624
        lon = -94.84304617526199
        display_name = "test"
        try:
            # self.updateData.lat = response[0]["lat"]
            # self.updateData.long = response[0]["lon"]
            # self.updateData.description = response[0]["display_name"]
            self.updateData.lat = lat
            self.updateData.long = lon
            self.updateData.description = display_name
            self.geocodeMessage = "\n\nGeocoding Success"
        except:
            self.geocodeMessage = "\n\nGeocoding Failed"

    def importClientParent(self):
        print("Client Search Initiated")
        try:
            print("Search Row Selected:", self.clientSearch.selectedRow)
            selectedClientSeries = self.clientDF.iloc[self.clientSearch.selectedRow]
            self.clientID = selectedClientSeries.client_id
            self.clientFirstNameValue.setText(selectedClientSeries.client_first_name)
            self.clientLastNameValue.setText(selectedClientSeries.client_last_name)
            self.clientPhoneValue.setText(selectedClientSeries.client_phone)
        except:
            print("Client Search Error")
            errorBox = QMessageBox()
            errorBox.setWindowTitle("Invalid Selection")
            errorBox.setText("Please ensure a row is selected before importing")
            errorBox.setIcon(QMessageBox.Critical)
            e = errorBox.exec_()

    def clearFields(self):
        self.propertyIDInput.setText("")
        # Address Inputs
        self.addNumInput.setText("")
        self.addNameInput.setText("")
        self.addCityInput.setText("")
        self.addZipInput.setText("")
        self.addStateInput.setText("Tx")
        # Client Inputs
        self.clientFirstNameValue.setText("")
        self.clientLastNameValue.setText("")
        self.clientPhoneValue.setText("")
        self.noteField.setText("")

    def createFields(self):
        print("Creating Fields")
        # Property Information
        startRowAdd = 5

        emptyRow1 = QLabel()
        emptyRow1.setText(" ")
        self.layout.addWidget(emptyRow1, startRowAdd - 3, 0)

        addNumLab = QLabel()
        addNumLab.setText("Street Number:")
        self.layout.addWidget(addNumLab, startRowAdd, 0)

        self.addNumInput.setMaximumWidth(100)
        self.layout.addWidget(self.addNumInput, startRowAdd + 1, 0)

        addNameLab = QLabel()
        addNameLab.setText("Street Name:")
        self.layout.addWidget(addNameLab, startRowAdd, 1)

        self.addNameInput.setMaximumWidth(200)
        self.layout.addWidget(self.addNameInput, startRowAdd + 1, 1)

        addCityLab = QLabel()
        addCityLab.setText("City:")
        self.layout.addWidget(addCityLab, startRowAdd + 2, 0)

        self.addCityInput.setMaximumWidth(200)
        self.layout.addWidget(self.addCityInput, startRowAdd + 3, 0)

        addZipLab = QLabel()
        addZipLab.setText("Zip:")
        self.layout.addWidget(addZipLab, startRowAdd + 2, 1)

        self.addZipInput.setMaximumWidth(100)
        self.layout.addWidget(self.addZipInput, startRowAdd + 3, 1)

        addStateLab = QLabel()
        addStateLab.setText("State:")
        self.layout.addWidget(addStateLab, startRowAdd + 2, 2)

        self.addStateInput.setMaximumWidth(50)
        self.addStateInput.setText("TX")
        self.layout.addWidget(self.addStateInput, startRowAdd + 3, 2)

        # Client Information
        startRowClient = startRowAdd + 7
        emptyRow1 = QLabel()
        emptyRow1.setText(" ")
        self.layout.addWidget(emptyRow1, startRowClient - 2, 0)

        clientFindButton = QPushButton()
        clientFindButton.setText("Search for Client")
        self.layout.addWidget(clientFindButton, startRowClient - 1, 0)
        clientFindButton.clicked.connect(self.findClient)

        clientFirstNameLab = QLabel()
        clientFirstNameLab.setText("Client First Name:")
        self.layout.addWidget(clientFirstNameLab, startRowClient, 0)

        self.clientFirstNameValue.setMaximumWidth(300)
        self.layout.addWidget(self.clientFirstNameValue, startRowClient + 1, 0)

        clientLastNameLab = QLabel()
        clientLastNameLab.setText("Client Last Name:")
        self.layout.addWidget(clientLastNameLab, startRowClient, 1)

        self.clientLastNameValue.setMaximumWidth(300)
        self.layout.addWidget(self.clientLastNameValue, startRowClient + 1, 1)

        clientPhoneLab = QLabel()
        clientPhoneLab.setText("Client Phone Number:")
        self.layout.addWidget(clientPhoneLab, startRowClient, 2)

        self.clientPhoneValue.setMaximumWidth(100)
        self.layout.addWidget(self.clientPhoneValue, startRowClient + 1, 2)

        noteLabel = QLabel()
        noteLabel.setText("Notes:")
        self.layout.addWidget(noteLabel, startRowClient + 2, 1)
        #noteField.setMinimumWidth(200)
        #noteField.setMinimumHeight(150)
        self.layout.addWidget(self.noteField, startRowClient + 3, 1)

        # Submit New Data Button
        self.submitButton.setText("Submit to Database")
        self.submitButton.setStyleSheet("background-color : lightblue")
        self.layout.addWidget(self.submitButton, startRowClient + 4, 3)

        # Clear Data Button
        clearButton = QPushButton()
        clearButton.setText("Clear all Entries")
        self.layout.addWidget(clearButton, 1, 4)
        clearButton.clicked.connect(self.clearFields)

    def updateParentData(self):
        print("Updating App propDF from NewJobTab propDF")


class ClientSearchWindow(QWidget):

    def __init__(self, clientDF):
        # super().__init__()
        QWidget.__init__(self)
        self.left = 200
        self.top = 200
        self.width = 700
        self.height = 300
        self.setGeometry(self.left, self.top, self.width, self.height)

        self.clientDF = clientDF.copy()
        self.searchDF = self.clientDF
        self.selectedRow = None
        self.title = 'Client Lookup'
        self.setWindowTitle(self.title)
        self.layout = QVBoxLayout(self)
        self.view = QTableView(self)
        self.header = self.view.horizontalHeader()

        # Search Bar
        self.searchLabel = QLabel()
        self.searchLabel.setText("Search:")
        self.layout.addWidget(self.searchLabel)
        self.clientSearchInput = QLineEdit()
        self.layout.addWidget(self.clientSearchInput)
        self.clientSearchInput.textChanged.connect(self.searchEvent)

        # Table
        self.view.resizeRowsToContents()
        self.view.resizeColumnsToContents()
        self.model = PandasModel(self.searchDF.head(100))
        self.view.setModel(self.model)
        self.view.setSelectionBehavior(QTableView.SelectRows)
        self.view.setSelectionMode(QTableView.SingleSelection)
        self.layout.addWidget(self.view)

        # Select Button
        self.selectButton = QPushButton()
        self.selectButton.setText("Select Client")
        self.layout.addWidget(self.selectButton)

        # Button Functions
        self.view.clicked.connect(self.rowClicked)
        self.selectButton.clicked.connect(self.importClientChild)

    def rowClicked(self):
        self.selectedRow = self.view.currentIndex()
        self.selectedRow = self.view.model().index(self.selectedRow.row(), 0).row()

    def importClientChild(self):
        self.destroy()

    def searchEvent(self):
        mask = np.column_stack(
            [self.clientDF[col].astype('str').str.contains(self.clientSearchInput.text().lower(), na=False, case=False)
             for col in self.clientDF])
        self.searchDF = self.clientDF.loc[mask.any(axis=1)]
        self.updateModel()

    def updateModel(self):
        self.model = PandasModel(self.searchDF.head(100))
        self.view.setModel(self.model)


class ClientManagementTab(QWidget):
    def __init__(self, clientDF):
        # super().__init__()
        QWidget.__init__(self)

        self.clientDF = clientDF.copy()
        self.searchDF = self.clientDF
        self.selectedRow = None
        self.selected_client_id = None

        self.layout = QVBoxLayout(self)
        self.view = QTableView(self)
        self.header = self.view.horizontalHeader()

        # Search Bar
        self.searchLabel = QLabel()
        self.searchLabel.setText("Search:")
        self.layout.addWidget(self.searchLabel)
        self.clientSearchInput = QLineEdit()
        self.clientSearchInput.setStyleSheet("background-color : white")
        self.clientSearchInput.setMinimumHeight(30)
        self.clientSearchInput.textChanged.connect(self.searchEvent)

        # New Client Button
        self.newClientButton = QPushButton()
        self.newClientButton.setText("New Client")
        self.newClientButton.setStyleSheet("background-color : lightgreen")
        self.newClientButton.clicked.connect(self.createEditLayout)

        # Edit Client
        self.clientEditButton = QPushButton()
        self.clientEditButton.setText("Edit Client")
        self.clientEditButton.setStyleSheet("background-color : lightblue")
        self.clientEditButton.clicked.connect(self.editClient)

        # Table
        self.view.resizeRowsToContents()
        self.view.resizeColumnsToContents()
        self.model = PandasModel(self.searchDF.head(100))
        self.view.setModel(self.model)
        self.view.setSortingEnabled(True)
        self.view.setSelectionBehavior(QTableView.SelectRows)
        self.view.setSelectionMode(QTableView.SingleSelection)
        self.view.resizeRowsToContents()
        self.view.resizeColumnsToContents()
        self.view.clicked.connect(self.rowClicked)

        # Add Default Layout
        self.resetLayout()
        # Client Edit Labels
        self.clientFirstNameLabel = QLabel()
        self.clientFirstNameLabel.setText("First Name")
        self.clientLastNameLabel = QLabel()
        self.clientLastNameLabel.setText("Last Name")
        self.clientPhoneLabel = QLabel()
        self.clientPhoneLabel.setText("Primary Phone")
        self.clientPhone2Label = QLabel()
        self.clientPhone2Label.setText("Secondary Phone")
        self.clientEmailLabel = QLabel()
        self.clientEmailLabel.setText("Email Address")

        self.emptySpace = QLabel()
        self.emptySpace.setText("  ")
        self.mailingLabel = QLabel()
        self.mailingLabel.setText("Client Mailing Address Information:")
        self.emptySpace2 = QLabel()
        self.emptySpace2.setText("  ")
        self.mailingLabel2 = QLabel()

        self.clientAddNumLabel = QLabel()
        self.clientAddNumLabel.setText("Street Number:")
        self.clientAddNum2Label = QLabel()
        self.clientAddNum2Label.setText("Street Number Extended:")
        self.clientAddNameLabel = QLabel()
        self.clientAddNameLabel.setText("Street Name:")
        self.clientAddCityLabel = QLabel()
        self.clientAddCityLabel.setText("City:")
        self.clientAddStateLabel = QLabel()
        self.clientAddStateLabel.setText("State:")
        self.clientAddZipLabel = QLabel()
        self.clientAddZipLabel.setText("Zip:")

        # Client Input Fields
        self.clientFirstNameInput = QLineEdit()
        self.clientLastNameInput = QLineEdit()
        self.clientPhoneInput = QLineEdit()
        self.clientPhone2Input = QLineEdit()
        self.clientEmailInput = QLineEdit()

        self.clientAddNumInput = QLineEdit()
        self.clientAddNum2Input = QLineEdit()
        self.clientAddNameInput = QLineEdit()
        self.clientAddCityInput = QLineEdit()
        self.clientAddStateInput = QLineEdit()
        self.clientAddZipInput = QLineEdit()

        # Validators
        numericRegex = QRegExp("[0-9].+")
        numericValidator = QRegExpValidator(numericRegex)
        self.clientAddNumInput.setValidator(numericValidator)
        self.clientAddNumInput.setValidator(numericValidator)
        self.clientAddZipInput.setInputMask("99999")
        self.clientPhoneInput.setInputMask("(000) 000-0000")
        self.clientAddStateInput.setInputMask("AA")

        # Submit Button
        self.submitClient = QPushButton()
        self.submitClient.setText("Submit")
        self.submitClient.setStyleSheet("background-color : lightblue")
        self.submitClient.clicked.connect(self.enterClient)
        # Cancel Button
        self.cancelClient = QPushButton()
        self.cancelClient.setText("Cancel")
        self.cancelClient.setStyleSheet("background-color : red")
        self.cancelClient.clicked.connect(self.resetLayout)

    def clearFields(self):
        self.clientFirstNameInput.setText("")
        self.clientLastNameInput.setText("")
        self.clientPhoneInput.setText("")
        self.clientPhone2Input.setText("")
        self.clientEmailInput.setText("")

        self.clientAddNumInput.setText("")
        self.clientAddNum2Input.setText("")
        self.clientAddNameInput.setText("")
        self.clientAddCityInput.setText("")
        self.clientAddStateInput.setText("")
        self.clientAddZipInput.setText("")

    def resetLayout(self):
        print("Resetting Layout")
        # remove widgets
        for i in reversed(range(self.layout.count())):
            self.layout.itemAt(i).widget().setParent(None)
        # Update Table
        self.searchDF = self.clientDF
        self.model = PandasModel(self.searchDF.head(100))
        self.view.setModel(self.model)
        # Add back in Widgets
        self.layout.addWidget(self.clientSearchInput)
        self.layout.addWidget(self.newClientButton)
        self.layout.addWidget(self.clientEditButton)
        self.layout.addWidget(self.view)

    def rowClicked(self):
        self.selectedRow = self.view.currentIndex()
        self.selectedRow = self.view.model().index(self.selectedRow.row(), 0).row()

    def searchEvent(self):
        mask = np.column_stack(
            [self.clientDF[col].astype('str').str.contains(self.clientSearchInput.text().lower(), na=False, case=False)
             for col in self.clientDF])
        self.searchDF = self.clientDF.loc[mask.any(axis=1)]
        self.updateModel()

    def updateModel(self):
        self.model = PandasModel(self.searchDF.head(100))
        self.view.setModel(self.model)

    def editClient(self):
        print("Edit Existing Client")
        if self.selectedRow is None:
            errorBox = QMessageBox()
            errorBox.setWindowTitle("No Selection")
            errorBox.setText("Please Select a Client to Edit by Clicking on a Row")
            errorBox.setIcon(QMessageBox.Critical)
            errorBox.exec_()
        else:
            selectedDat = self.clientDF.iloc[self.selectedRow].copy()
            self.selected_client_id = selectedDat.client_id
            selectedDat.fillna("", inplace=True)
            selectedDat = selectedDat.astype('str')

            self.createEditLayout()
            self.clientFirstNameInput.setText(selectedDat.client_first_name)
            self.clientLastNameInput.setText(selectedDat.client_last_name)
            self.clientPhoneInput.setText(selectedDat.client_phone)
            self.clientPhone2Input.setText(selectedDat.client_phone_2)
            self.clientEmailInput.setText(selectedDat.client_email)
            self.clientAddNumInput.setText(selectedDat.client_address_number)
            self.clientAddNum2Input.setText(selectedDat.client_address_2)
            self.clientAddNameInput.setText(selectedDat.client_address_street)
            self.clientAddCityInput.setText(selectedDat.client_address_city)
            self.clientAddStateInput.setText(selectedDat.client_address_state)
            self.clientAddZipInput.setText(selectedDat.client_address_zip)

    def enterClient(self):
        # print(self.selected_client_id)
        updateData = pd.DataFrame({"client_id": self.selected_client_id,
                                   "client_first_name": self.clientFirstNameInput.text(),
                                   "client_last_name": self.clientLastNameInput.text(),
                                   "client_phone": self.clientPhoneInput.text(),
                                   "client_phone_2": self.clientPhone2Input.text(),
                                   "client_email": self.clientEmailInput.text(),
                                   "client_address_number": self.clientAddNumInput.text(),
                                   "client_address_2": self.clientAddNum2Input.text(),
                                   "client_address_street": self.clientAddNameInput.text(),
                                   "client_address_city": self.clientAddCityInput.text(),
                                   "client_address_state": self.clientAddStateInput.text(),
                                   "client_address_zip": self.clientAddZipInput.text()
                                   }, index=[0])
        emptyInputs = updateData.columns[updateData.isin([""]).any()].tolist()
        mandatoryInputs = ["client_first_name", "client_last_name", "client_phone", "client_address_number",
                           "client_address_city", "client_address_state", "client_address_zip"]
        if any(x in emptyInputs for x in mandatoryInputs):
            print("Missing Client Data Error")
            errorBox = QMessageBox()
            errorBox.setWindowTitle("Missing Fields")
            errorBox.setText("Please Ensure Client Name and Address are Filled")
            errorBox.setIcon(QMessageBox.Critical)
            errorBox.exec_()
        else:
            if self.selected_client_id is not None:
                print("Updating Existing Client")
                exInd = self.clientDF[self.clientDF.client_id == self.selected_client_id].index.item()
                self.clientDF.iloc[exInd] = updateData.iloc[0]
            else:
                print("Creating New Client")
                self.clientDF = pd.concat([self.clientDF, updateData])
                self.clientDF.reset_index(drop=True, inplace=True)
                self.clientDF.at[len(self.clientDF) - 1, "client_id"] = self.clientDF.client_id[
                                                                            len(self.clientDF) - 2] + 1
            self.clearFields()
            self.resetLayout()

    def createEditLayout(self):
        startRow = 1
        print("Creating Client Edit Layout")
        for i in reversed(range(self.layout.count())):
            self.layout.itemAt(i).widget().setParent(None)

        # Add Labels and Input Fields
        self.layout.addWidget(self.clientFirstNameLabel)
        self.layout.addWidget(self.clientFirstNameInput)
        self.layout.addWidget(self.clientLastNameLabel)
        self.layout.addWidget(self.clientLastNameInput)
        self.layout.addWidget(self.clientPhoneLabel)
        self.layout.addWidget(self.clientPhoneInput)
        self.layout.addWidget(self.clientPhone2Label)
        self.layout.addWidget(self.clientPhone2Input)
        self.layout.addWidget(self.clientEmailLabel)
        self.layout.addWidget(self.clientEmailInput)
        self.layout.addWidget(self.emptySpace)
        self.layout.addWidget(self.mailingLabel)
        self.layout.addWidget(self.emptySpace2)
        self.layout.addWidget(self.clientAddNumLabel)
        self.layout.addWidget(self.clientAddNumInput)
        self.layout.addWidget(self.clientAddNum2Label)
        self.layout.addWidget(self.clientAddNum2Input)
        self.layout.addWidget(self.clientAddNameLabel)
        self.layout.addWidget(self.clientAddNameInput)
        self.layout.addWidget(self.clientAddCityLabel)
        self.layout.addWidget(self.clientAddCityInput)
        self.layout.addWidget(self.clientAddStateLabel)
        self.layout.addWidget(self.clientAddStateInput)
        self.layout.addWidget(self.clientAddZipLabel)
        self.layout.addWidget(self.clientAddZipInput)

        self.clientAddStateInput.setText("Tx")
        # Include Buttons
        self.layout.addWidget(self.submitClient)
        self.layout.addWidget(self.cancelClient)


class JobViewTab(QWidget):
    def __init__(self, propDF):
        # super().__init__()
        QWidget.__init__(self)

        self.mapOutput = QtWebEngineWidgets.QWebEngineView()

        self.layout = QVBoxLayout(self)


        notStarted = QLabel()
        notStarted.setText("Not Started")
        notStarted.setStyleSheet('color: red')
        self.layout.addWidget(notStarted)
        quoted = QLabel()
        quoted.setText("Quoted")
        quoted.setStyleSheet('color: orange')
        self.layout.addWidget(quoted)
        invoiced = QLabel()
        invoiced.setText("Invoiced")
        invoiced.setStyleSheet('color: green')
        self.layout.addWidget(invoiced)
        preliminary = QLabel()
        preliminary.setText("Preliminary")
        preliminary.setStyleSheet('color: lightgreen')
        self.layout.addWidget(preliminary)
        fieldWorkReady = QLabel()
        fieldWorkReady.setText("Field Work Ready")
        fieldWorkReady.setStyleSheet('color: pink')
        self.layout.addWidget(fieldWorkReady)
        fieldWorkComp = QLabel()
        fieldWorkComp.setText("Field Work Complete")
        fieldWorkComp.setStyleSheet('color: purple')
        self.layout.addWidget(fieldWorkComp)
        drafted = QLabel()
        drafted.setText("Drafted")
        drafted.setStyleSheet('color: darkblue')
        self.layout.addWidget(drafted)
        finalized = QLabel()
        finalized.setText("Finalized")
        finalized.setStyleSheet('color: grey')
        self.layout.addWidget(finalized)


        spacer = QSpacerItem(20, 40, QSizePolicy.Minimum, QSizePolicy.Expanding)
        self.mapOutput.resize(200, 1000)
        self.layout.addWidget(self.mapOutput)
        self.layout.addItem(spacer)

        self.propDF = propDF
        self.map = folium.Map(location=[29.407786779390726, -94.9356243977614])
        self.createMap()

    def createMap(self):
        # print("Creating Map")
        # print(self.propDF)
        mapDF = self.propDF[["lat", "long", "description", "status"]].copy()
        mapDF["description"].fillna("", inplace=True)
        mapDF.dropna(inplace=True)

        colorDict = {"Not Started": "red",
                     "Quoted": "orange",
                     "Invoiced": "green",
                     "Preliminary": "lightgreen",
                     "Field Work Ready": "lightred",
                     "Field Work Complete": "purple",
                     "Drafted": "darkblue",
                     "Finalized": "lightgray"
                     }

        self.map = folium.Map(location=[29.407786779390726, -94.9356243977614])
        folium.Marker(
            location=[29.407786779390726, -94.9356243977614],  # coordinates for the marker
            popup="Ellis Landsurveying Office (Where the most wonderful and sexy Land Surveyor can be Found most days)",  # pop-up label for the marker
            icon=folium.Icon(),
            color="red"
        ).add_to(self.map)
        # mapDF.apply(lambda row: folium.Marker(location=[row["lat"], row["long"]], color="green",
        #                                       radius=5, popup=row['description']).add_to(self.map), axis=1)
        mapDF.apply(lambda row: folium.Marker([row["lat"], row["long"]],
                                              popup=row["description"],
                                              icon=folium.Icon(color=colorDict[row['status']])).add_to(self.map),
                    axis=1)
        # folium.Marker([row[lat], row[lon]], popup=row("description), icon=folium.Icon(color=colorDict[row['status']))
        # self.map.save('tempMap.html')
        self.map.add_child(folium.ClickForMarker(popup=None))
        self.mapOutput.setHtml(self.map._repr_html_())


class JobManagementTab(QWidget):
    def __init__(self, propDF):
        # super().__init__()
        QWidget.__init__(self)

        self.propDF = propDF
        self.selectedRow = None
        self.selected_client_id = None

        self.layout = QVBoxLayout(self)
        self.view = QTableView(self)
        self.header = self.view.horizontalHeader()

        # Search Bar
        self.searchLabel = QLabel()
        self.searchLabel.setText("Search:")
        self.layout.addWidget(self.searchLabel)
        self.propSearchInput = QLineEdit()
        self.propSearchInput.setStyleSheet("background-color : white")
        self.propSearchInput.setMinimumHeight(30)

        # Table
        self.model = PandasModel(self.propDF)
        self.view.setSelectionBehavior(QTableView.SelectRows)
        self.view.setSelectionMode(QTableView.SingleSelection)
        self.view.clicked.connect(self.rowClicked)

        # Update Table
        self.proxyModel = QSortFilterProxyModel()
        self.proxyModel.setFilterKeyColumn(-1)
        self.proxyModel.setSourceModel(self.model)
        self.view.setModel(self.proxyModel)
        #self.view.setSortingEnabled(True)
        # self.proxyModel.sort(0, Qt.DescendingOrder)
        self.propSearchInput.textChanged.connect(self.proxyModel.setFilterFixedString)
        self.propSearchInput.textChanged.connect(self.searchEvent)
        #self.view.resizeRowsToContents()
        self.view.resizeColumnsToContents()
        self.view.setColumnWidth(self.propDF.columns.get_loc("notes"), 220)
        self.view.setColumnWidth(self.propDF.columns.get_loc("status"), 150)

        self.proxyModel.setFilterCaseSensitivity(False)
        # Add back in Widgets
        self.layout.addWidget(self.propSearchInput)
        self.layout.addWidget(self.view)
        self.setEditableFields(self.propDF.index.to_list())


    def setEditableFields(self, rows):
        for col in range(0, len(self.propDF.columns)):
            for row in rows:
                modelIndex = self.view.model().index(row, col)
                if self.propDF.columns[col] == "status":
                    combo = QComboBox()
                    combo.addItems(["Not Started", "Quoted", "Invoiced", "Preliminary", "Field Work Ready", "Field Work Complete", "Drafted", "Finalized"])
                    combo.setCurrentText(self.propDF.status[row])
                    combo.currentTextChanged.connect(self.updateData)
                    self.view.setIndexWidget(self.view.model().index(row, col), combo)
                    self.view.model().setData(modelIndex, str(self.propDF.iloc[row][col]))

                elif self.propDF.columns[col] in ["lat", "long", "description", "notes"]:
                    lineEdit = QLineEdit()
                    lineEdit.setText(str(self.propDF.iloc[row][col]))
                    lineEdit.returnPressed.connect(self.updateLineData)
                    self.view.setIndexWidget(self.view.model().index(row, col), lineEdit)
                    self.view.model().setData(modelIndex, str(self.propDF.iloc[row][col]))

                else:
                    self.view.model().setData(modelIndex, str(self.propDF.iloc[row][col]))

    def updateLineData(self):

        rowChanged = self.view.currentIndex().row()
        columnChanged = self.view.currentIndex().column()
        widget = self.view.indexWidget(self.view.model().index(rowChanged, columnChanged))
        widgetIndex = self.view.model().index(rowChanged, columnChanged)
        text = widget.text()

        errorBox = QMessageBox()
        errorBox.setIcon(QMessageBox.Critical)
        errorBox.setWindowTitle("Error")
        errorBox.setText("Data Type Error. Ensure Lat and Long only contain numerical values")

        confirmBox = QMessageBox()
        confirmBox.setWindowTitle("Confirm Changes")
        confirmBox.setText("Please Confirm Changes")
        confirmBox.setIcon(QMessageBox.Question)
        confirmBox.setStandardButtons(QMessageBox.Save | QMessageBox.Discard)
        confirmBox.setDefaultButton(QMessageBox.Save)
        confirmBoxResp = confirmBox.exec_()


        if confirmBoxResp == 2048:
            if self.propDF.columns[columnChanged] in ["lat", "long"]:
                try:
                    float(text)
                    print('here')
                except:
                    errorBox = QMessageBox()
                    errorBox.setIcon(QMessageBox.Critical)
                    errorBox.setWindowTitle("Error")
                    errorBox.setText("Data Type Error. Ensure Lat and Long only contain numerical values")
                    errorBox.exec_()
                    text = self.view.model().data(widgetIndex)

            #---Change By User----
            propModelIndex = self.view.model().index(rowChanged, 0)
            propID = self.view.model().data(propModelIndex)
            propDFIndex = self.propDF[self.propDF.property_id.astype('str') == str(propID)].index[0]
            self.propDF.at[propDFIndex, self.propDF.columns[columnChanged]] = text

            changeIndex = self.view.model().index(propDFIndex, columnChanged)
            #print("propID:", propDFIndex)
            #print("New Text: ", text)
            self.view.model().setData(changeIndex, text)
            #print("Model Status:", self.view.model().setData(changeIndex, text))
            #print("Model Data:", self.view.model().data(changeIndex))

            #---Updated Last Changed By Column---
            changeByCol = self.propDF.columns.get_loc("last_changed_by")
            self.propDF.at[propDFIndex, self.propDF.columns[changeByCol]] = os.getlogin()
            changeByIndex = self.view.model().index(rowChanged, changeByCol)
            print("old value:", self.view.model().data(changeByIndex))
            self.view.model().setData(changeByIndex, os.getlogin())
            print("New value:", self.view.model().data(changeByIndex))



        self.searchEvent()

    def updateData(self, text):

        # Check if Invoice is Being Changed
        if text == "Invoiced":
            confirmBox = QMessageBox()
            confirmBox.setWindowTitle("Confirm Changes")
            confirmBox.setText("This will Update Invoice Date. Please Confirm Changes")
            confirmBox.setIcon(QMessageBox.Question)
            confirmBox.setStandardButtons(QMessageBox.Save | QMessageBox.Discard)
            confirmBox.setDefaultButton(QMessageBox.Save)
            confirmBoxResp = confirmBox.exec_()
        else:
            confirmBoxResp = 2048

        if confirmBoxResp == 2048:
            combobox = self.sender()
            ix = self.view.indexAt(combobox.pos())
            rowChanged = ix.row()
            columnChanged = ix.column()
            propModelIndex = self.view.model().index(rowChanged, 0)
            propID = self.view.model().data(propModelIndex)
            propDFIndex = self.propDF[self.propDF.property_id.astype('str') == str(propID)].index[0]
            self.propDF.at[propDFIndex, self.propDF.columns[columnChanged]] = text

            changeIndex = self.view.model().index(propDFIndex, columnChanged)
            #print("propID:", propDFIndex)
            #print("New Text: ", text)
            self.view.model().setData(changeIndex, text)
            #print("Model Status:", self.view.model().setData(changeIndex, text))
            #print("Model Data:", self.view.model().data(changeIndex))

            #Update User
            # ---Updated Last Changed By Column---
            changeByCol = self.propDF.columns.get_loc("last_changed_by")
            self.propDF.at[propDFIndex, self.propDF.columns[changeByCol]] = os.getlogin()
            changeByIndex = self.view.model().index(rowChanged, changeByCol)
            #print("old value:", self.view.model().data(changeByIndex))
            self.view.model().setData(changeByIndex, os.getlogin())
            #print("New value:", self.view.model().data(changeByIndex))

            #Update Invoice Date
            if text == "Invoiced":
                invoiceCol = self.propDF.columns.get_loc("invoice_date")
                self.propDF.at[propDFIndex, self.propDF.columns[invoiceCol]] = str(datetime.date.today())
                invoiceIndex = self.view.model().index(rowChanged, invoiceCol)
                # print("old value:", self.view.model().data(changeByIndex))
                self.view.model().setData(invoiceIndex, str(datetime.date.today()))
                # print("New value:", self.view.model().data(changeByIndex))


        self.searchEvent()

    def rowClicked(self):
        self.selectedRow = self.view.currentIndex()
        self.selectedRow = self.view.model().index(self.selectedRow.row(), 0).row()

    def searchEvent(self):
        searchDF = self.propDF[
            self.propDF.apply(lambda row: row.astype(str).str.contains(self.propSearchInput.text(), case=False).any(),
                              axis=1)]
        self.setEditableFields(searchDF.index.to_list())


class CentralWidget(QWidget):
    def __init__(self, widget):
        super().__init__()
        self._widget = widget
        self.widget.setParent(self)

    @property
    def widget(self):
        return self._widget

    def resizeEvent(self, event):
        super().resizeEvent(event)
        size = min(self.width(), self.height())
        r = QStyle.alignedRect(
            Qt.LeftToRight, Qt.AlignLeft, QSize(size, 400), self.rect()
        )
        self.widget.setGeometry(r)


class PandasModel(QtCore.QAbstractTableModel):
    """
    Class to populate a table view with a pandas dataframe
    """

    def __init__(self, data, parent=None):
        QtCore.QAbstractTableModel.__init__(self, parent)
        self.dataDF = data.copy()
        self._data = np.array(data.values)
        self._cols = data.columns
        self.r, self.c = np.shape(self._data)

    def rowCount(self, parent=None):
        return self.r

    def columnCount(self, parent=None):
        return self.c

    def data(self, index, role=Qt.DisplayRole):
        if (index.row() <= self.r) and (index.column() <= self.c):
            if index.isValid():
                if role == Qt.DisplayRole:
                    if str(self.dataDF.dtypes[index.column()]).find('int') != -1:
                        return int(self._data[index.row(), index.column()])
                    elif str(self.dataDF.dtypes[index.column()]).find('float') != -1:
                        return float(self._data[index.row(), index.column()])
                    else:
                        return str(self._data[index.row(), index.column()])
        return None

    def headerData(self, p_int, orientation, role):
        if role == Qt.DisplayRole:
            if orientation == Qt.Horizontal:
                return self._cols[p_int]
            elif orientation == Qt.Vertical:
                return p_int
        return None

    def setData(self, index, value, role=QtCore.Qt.EditRole):
        if role == QtCore.Qt.EditRole:
            row = index.row()
            column = index.column()
            self._data[row][column] = value
            self.dataChanged.emit(index, index)
            return True
        return QtCore.QAbstractTableModel.setData(self, index, value, role)

    # def sort(self, column, order=Qt.AscendingOrder):
    #     print('sort clicked col {} order {}'.format(column, order))
    #     self._data = self._data[self._data[:, column].argsort()][::-1] #order == Qt.AscendingOrder
    #     self.layoutAboutToBeChanged.emit()
    #     self.layoutChanged.emit()


if __name__ == '__main__':
    app = QApplication(sys.argv)
    ex = App()
    sys.exit(app.exec_())
