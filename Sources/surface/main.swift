import Foundation

let vendorId: UInt16 = 1430
let productId: UInt16 = 1

EXIIDeviceHandler
    .init()
    .start()

USBDeviceDaemon
    .init(vendorId: vendorId, productId: productId)
    .start()

RunLoop.current.run()

/*
 Example 1: https://github.com/opensource-apple/IOUSBFamily/blob/7fec39743a5f05254c7e5366d9b0223ecc5dce71/Examples/USBSimple%20Example/main.c
 
 */
