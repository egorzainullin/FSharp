namespace Homework6

module Factory =

    open Homework6
    open Net

    let CreateWindows isInfected = Machine("Windows", 0.8, isInfected)

    let CreateLinux isInfected = Machine("Linux", 0.4, isInfected)
    
    let CreateMac isInfected = Machine("Mac", 0.2, isInfected)
    

