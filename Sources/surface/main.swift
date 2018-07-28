import Foundation

let vendorId: UInt16 = 1430
let productId: UInt16 = 1

EXIIDeviceHandler
    .init()
    .start()

USBDeviceDaemon
    .init(vendorId: vendorId, productId: productId)
    .start()

try SocketServer
    .init()
    .run()

RunLoop.current.run()
