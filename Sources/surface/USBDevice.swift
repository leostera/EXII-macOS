import Cocoa
import Foundation
import IOKit
import IOKit.usb
import IOKit.usb.IOUSBLib

public let kIOUSBInterfaceUserClientTypeID: CFUUID = CFUUIDGetConstantUUIDWithBytes(kCFAllocatorDefault,
                                                                                     0x2d, 0x97, 0x86, 0xc6,
                                                                                     0x9e, 0xf3, 0x11, 0xD4,
                                                                                     0xad, 0x51, 0x00, 0x0a,
                                                                                     0x27, 0x05, 0x28, 0x61)
public let kIOUSBInterfaceInterfaceID: CFUUID =  CFUUIDGetConstantUUIDWithBytes(kCFAllocatorDefault,
                                                                                 0x73, 0xc9, 0x7a, 0xe8,
                                                                                 0x9e, 0xf3, 0x11, 0xD4,
                                                                                 0xb1, 0xd0, 0x00, 0x0a,
                                                                                 0x27, 0x05, 0x28, 0x61)
public let kIOUSBDeviceUserClientTypeID: CFUUID = CFUUIDGetConstantUUIDWithBytes(kCFAllocatorDefault,
                                                                                  0x9d, 0xc7, 0xb7, 0x80,
                                                                                  0x9e, 0xc0, 0x11, 0xD4,
                                                                                  0xa5, 0x4f, 0x00, 0x0a,
                                                                                  0x27, 0x05, 0x28, 0x61)
public let kIOUSBDeviceInterfaceID: CFUUID =  CFUUIDGetConstantUUIDWithBytes(kCFAllocatorDefault,
                                                                              0x5c, 0x81, 0x87, 0xd0,
                                                                              0x9e, 0xf3, 0x11, 0xD4,
                                                                              0x8b, 0x45, 0x00, 0x0a,
                                                                              0x27, 0x05, 0x28, 0x61)
public let kIOCFPlugInInterfaceID: CFUUID =  CFUUIDGetConstantUUIDWithBytes(kCFAllocatorDefault,
                                                                             0xC2, 0x44, 0xE8, 0x58,
                                                                             0x10, 0x9C, 0x11, 0xD4,
                                                                             0x91, 0xD4, 0x00, 0x50,
                                                                             0xE4, 0xC6, 0x42, 0x6F)

public extension Notification.Name {
    static let USBDeviceConnected = Notification.Name("USBDeviceConnected")
    static let USBDeviceDisconnected = Notification.Name("USBDeviceDisconnected")
}

public struct USBDevicePipe {
    var pipeRef: UInt8
    var direction: UInt8
    var number: UInt8
    var transferType: UInt8
    var maxPacketSize: UInt16
    var interval: UInt8

    public init(
        pipeRef: UInt8,
        direction: UInt8,
        number: UInt8,
        transferType: UInt8,
        maxPacketSize: UInt16,
        interval: UInt8
    ) {
        self.pipeRef = pipeRef
        self.direction = direction
        self.number = number
        self.transferType = transferType
        self.maxPacketSize = maxPacketSize
        self.interval = interval
    }
}

public struct USBDevice {
    public let deviceId: UInt64
    public let locationId: UInt32
    public let vendorId: UInt16
    public let productId: UInt16
    
    public let pipes: [USBDevicePipe]

    public let name: String

    public let interfaceInterface: IOUSBInterfaceInterface
    public let interfaceInterfacePtrPtr: UnsafeMutablePointer<UnsafeMutablePointer<IOUSBInterfaceInterface>?>?

    public let deviceInterface: IOUSBDeviceInterface
    public let deviceInterfacePtrPtr: UnsafeMutablePointer<UnsafeMutablePointer<IOUSBDeviceInterface>?>?

    public init(deviceId: UInt64,
                locationId: UInt32,
                vendorId: UInt16,
                productId: UInt16,
                name: String,
                pipes: [USBDevicePipe],
                interfaceInterfacePtrPtr: UnsafeMutablePointer<UnsafeMutablePointer<IOUSBInterfaceInterface>?>?,
                deviceInterfacePtrPtr: UnsafeMutablePointer<UnsafeMutablePointer<IOUSBDeviceInterface>?>?) {
        self.deviceId = deviceId
        self.locationId = locationId
        self.vendorId = vendorId
        self.productId = productId
        self.name = name
        self.pipes = pipes
        self.interfaceInterface = (interfaceInterfacePtrPtr?.pointee!.pointee)!
        self.interfaceInterfacePtrPtr = interfaceInterfacePtrPtr
        self.deviceInterface = (deviceInterfacePtrPtr?.pointee!.pointee)!
        self.deviceInterfacePtrPtr = deviceInterfacePtrPtr
    }
}

func extractName(_ usbDevice: io_object_t) -> String? {
    // io_name_t imports to swift as a tuple (Int8, ..., Int8) 128 ints
    // although in device_types.h it's defined:
    // typedef    char io_name_t[128];
    var deviceNameCString: [CChar] = [CChar](repeating: 0, count: 128)
    let kr = IORegistryEntryGetName(usbDevice, &deviceNameCString)
    if kr != kIOReturnSuccess {
        print("Error getting device name")
        return Optional.none
    }
    return String(cString: &deviceNameCString)
}
