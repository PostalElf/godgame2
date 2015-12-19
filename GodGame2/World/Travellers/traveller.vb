Public Interface traveller
    Property name As String
    Property empire As empire
    Property travelDestination As travelLocation
    Property travelLocation As travelLocation
    Property travelCost As Integer
    Property travelProgress As Integer
    Property travelSpeed As Integer

    Sub moveTo(aDestination As travelLocation)
    Sub teleportTo(aDestination As travelLocation)
    Function getTravelCost(aOrigin As travelLocation, aDestination As travelLocation) As Integer
End Interface
