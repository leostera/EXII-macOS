import Cocoa
import Foundation
import IOKit
import IOKit.usb
import IOKit.usb.IOUSBLib

class USBDeviceDaemon {
    var vendorId: UInt16
    var productId: UInt16
    
    init(vendorId: UInt16, productId: UInt16) {
        self.productId = productId
        self.vendorId = vendorId
    }
    
    @objc open func onDeviceAdded(_ iterator: io_iterator_t) {
        while case let usbDevice = IOIteratorNext(iterator), usbDevice != 0 {
            // Common flow control variables
            var returnCode: Int32 = 0
            
            // Get device identifier
            var deviceId: UInt64 = 0
            if IORegistryEntryGetRegistryEntryID(usbDevice, &deviceId) != kIOReturnSuccess {
                print(Utils.now(), "Error getting device id", usbDevice)
                break
            }
            
            // Get device name
            let name = extractName(usbDevice)!
            
            /**
             *
             * Device Interface Dance!
             *
             */
            
            // These pointers are necessary to interact with the low-level C-style APIs
            var deviceInterfacePtrPtr: UnsafeMutablePointer<UnsafeMutablePointer<IOUSBDeviceInterface>?>?
            var devicePlugInInterfacePtrPtr: UnsafeMutablePointer<UnsafeMutablePointer<IOCFPlugInInterface>?>?
            
            // We are not using the `score` at the moment, but it's required to create a plug-in interface
            var deviceScore: Int32 = 0
            
            // Get plugInInterface for current USB device
            returnCode = IOCreatePlugInInterfaceForService(
                usbDevice,
                kIOUSBDeviceUserClientTypeID,
                kIOCFPlugInInterfaceID,
                &devicePlugInInterfacePtrPtr,
                &deviceScore
            )
            if returnCode != kIOReturnSuccess {
                print("Unable to create PlugIn Interface for Device:", returnCode)
                continue
            }
            guard let devicePlugInInterface = devicePlugInInterfacePtrPtr?.pointee?.pointee else {
                print("Unable to dereference Plug-In Interface for Device")
                continue
            }
            
            // Get a device interface by querying the plugInInterface
            returnCode = withUnsafeMutablePointer(to: &deviceInterfacePtrPtr) {
                $0.withMemoryRebound(to: Optional<LPVOID>.self, capacity: 1) {
                    devicePlugInInterface.QueryInterface(
                        devicePlugInInterfacePtrPtr,
                        CFUUIDGetUUIDBytes(kIOUSBDeviceInterfaceID),
                        $0
                    )
                }
            }
            IODestroyPlugInInterface(devicePlugInInterfacePtrPtr)
            guard returnCode == kIOReturnSuccess else {
                print("Unable to query for Device Interface:", returnCode)
                continue
            }
            guard let deviceInterface = deviceInterfacePtrPtr?.pointee?.pointee else {
                print("Unable to get Device Interface")
                continue
            }
            returnCode = deviceInterface.USBDeviceOpen(deviceInterfacePtrPtr)
            if returnCode != kIOReturnSuccess && returnCode != kIOReturnExclusiveAccess {
                print("Could not open device (error: \(returnCode))")
                continue
            }
            
            /**
             *
             * Configure device
             *
             */
            var numConfig: UInt8 = 0
            returnCode = deviceInterface.GetNumberOfConfigurations(deviceInterfacePtrPtr, &numConfig)
            if (returnCode != kIOReturnSuccess) {
                print("unable to get number of configurations")
                continue
            }
            
            var configDesc: IOUSBConfigurationDescriptorPtr?
            returnCode = deviceInterface.GetConfigurationDescriptorPtr(deviceInterfacePtrPtr, 0, &configDesc)
            if (returnCode != kIOReturnSuccess) {
                print("unable to get config description for config 0 (index)")
                continue
            }

            returnCode = deviceInterface.SetConfiguration(deviceInterfacePtrPtr, configDesc!.pointee.bConfigurationValue)
            if (returnCode != kIOReturnSuccess) {
                print("unable to set configuration to config 0 (index)")
                continue
            }
            
            
            /**
             *
             * Interface Interface Dance!
             *
             * No, not a typo.
             */
            
            // Let's find all the interfaces first
            let interfaceRequest = IOUSBFindInterfaceRequest(
                bInterfaceClass: UInt16(kIOUSBFindInterfaceDontCare),
                bInterfaceSubClass: UInt16(kIOUSBFindInterfaceDontCare),
                bInterfaceProtocol: UInt16(kIOUSBFindInterfaceDontCare),
                bAlternateSetting: UInt16(kIOUSBFindInterfaceDontCare)
            )
            var interfaceIterator: io_iterator_t = 0
            
            returnCode = deviceInterface.CreateInterfaceIterator(
                deviceInterfacePtrPtr,
                UnsafeMutablePointer<IOUSBFindInterfaceRequest>(mutating: [interfaceRequest]),
                &interfaceIterator
            )
            if returnCode != kIOReturnSuccess {
                print("Could not get interface iterator:", returnCode)
                continue
            }
            
            // These pointers are necessary to interact with the low-level C-style APIs
            var interfaceInterfacePtrPtr: UnsafeMutablePointer<UnsafeMutablePointer<IOUSBInterfaceInterface>?>?
            
            while true {
                let usbInterface: io_service_t = IOIteratorNext(interfaceIterator)
                guard 0 < usbInterface else {
                    print("Could not get a usb interface from the interface iterator")
                    break
                }
                
                var interfacePlugInInterfacePtrPtr: UnsafeMutablePointer<UnsafeMutablePointer<IOCFPlugInInterface>?>?
                
                // We are not using the `score` at the moment, but it's required to create a plug-in interface
                var interfaceScore: Int32 = 0
                
                // Get plugInInterface for the first USB interface
                returnCode = IOCreatePlugInInterfaceForService(
                    usbInterface,
                    kIOUSBInterfaceUserClientTypeID,
                    kIOCFPlugInInterfaceID,
                    &interfacePlugInInterfacePtrPtr,
                    &interfaceScore
                )
                if returnCode != kIOReturnSuccess {
                    print("Unable to create PlugIn Interface for Interface:", returnCode)
                    continue
                }
                guard let interfacePlugInInterface = interfacePlugInInterfacePtrPtr?.pointee?.pointee else {
                    print("Unable to dereference Plug-In Interface for Interface")
                    continue
                }
                
                // Get a device's interface interface
                returnCode = withUnsafeMutablePointer(to: &interfaceInterfacePtrPtr) {
                    $0.withMemoryRebound(to: Optional<LPVOID>.self, capacity: 1) {
                        interfacePlugInInterface.QueryInterface(
                            interfacePlugInInterfacePtrPtr,
                            CFUUIDGetUUIDBytes(kIOUSBInterfaceInterfaceID),
                            $0
                        )
                    }
                }
                IODestroyPlugInInterface(interfacePlugInInterfacePtrPtr)
                if returnCode != kIOReturnSuccess {
                    print("Unable to query for Interface Interface:", returnCode)
                    continue
                }
            }
            
            guard let interfaceInterface = interfaceInterfacePtrPtr?.pointee?.pointee else {
                print("Unable to get Interface Interface")
                continue
            }
            returnCode = interfaceInterface.USBInterfaceOpen(interfaceInterfacePtrPtr)
            if returnCode != kIOReturnSuccess && returnCode != kIOReturnExclusiveAccess {
                print("Could not open device (error: \(returnCode))")
                continue
            }
            
            var numEndpoints: UInt8 = 0
            returnCode = interfaceInterface.GetNumEndpoints(interfaceInterfacePtrPtr, &numEndpoints)
            if returnCode != kIOReturnSuccess {
                print("Could not get number of endpoints:", returnCode)
                continue
            }
            
            
            
            print(Utils.now(), "Scanning for", numEndpoints, "pipes...")
            
            var pipes: [USBDevicePipe] = []
            for e in 1...numEndpoints {
                var direction: UInt8 = 0
                var number: UInt8 = 0
                var transferType: UInt8 = 0
                var maxPacketSize: UInt16 = 0
                var interval: UInt8 = 0
                
                returnCode = interfaceInterface.GetPipeProperties(
                    interfaceInterfacePtrPtr,
                    e,
                    &direction,
                    &number,
                    &transferType,
                    &maxPacketSize,
                    &interval
                )
                if returnCode != kIOReturnSuccess {
                    print("Could not get properties for pipe", e)
                    continue
                }
                
                let pipe = USBDevicePipe(
                    pipeRef: e,
                    direction: direction,
                    number: number,
                    transferType: transferType,
                    maxPacketSize: maxPacketSize,
                    interval: interval
                )
                
                print(Utils.now(), "- Found Pipe", pipe)
                pipes.append(pipe)
            }
            
            /**
             *
             * Now we can get some data from the device!
             *
             */
            
            var vendorId: UInt16 = 0
            var productId: UInt16 = 0
            var locationId: UInt32 = 0
            
            // Get LocationID from device
            returnCode = deviceInterface.GetLocationID(deviceInterfacePtrPtr, &locationId)
            if returnCode != kIOReturnSuccess {
                continue
            }
            
            // Get VendorID from device
            returnCode = deviceInterface.GetDeviceVendor(deviceInterfacePtrPtr, &vendorId)
            if returnCode != kIOReturnSuccess {
                continue
            }
            
            // Get ProductID from device
            returnCode = deviceInterface.GetDeviceProduct(deviceInterfacePtrPtr, &productId)
            if returnCode != kIOReturnSuccess {
                continue
            }
            
            // Build USBDevice
            let device = USBDevice(
                deviceId: deviceId,
                locationId: locationId,
                vendorId: vendorId,
                productId: productId,
                name: name,
                pipes: pipes,
                interfaceInterfacePtrPtr: interfaceInterfacePtrPtr,
                deviceInterfacePtrPtr: deviceInterfacePtrPtr
            )
            
            // Notify that we have a new device connected
            NotificationCenter.default.post(name: .USBDeviceConnected, object: [
                "device": device
            ])
        }
    }
    
    open func onDeviceRemoved(_ iterator: io_iterator_t) {
        while case let usbDevice = IOIteratorNext(iterator), usbDevice != 0 {
            var returnCode: Int32 = 0
            var deviceId: UInt64 = 0
            let name = extractName(usbDevice)!
            
            returnCode = IORegistryEntryGetRegistryEntryID(usbDevice, &deviceId)
            if returnCode != kIOReturnSuccess {
                print("Error getting device id")
            }
            returnCode = IOObjectRelease(usbDevice)
            if returnCode != kIOReturnSuccess {
                print("Couldn’t release raw device object (error: \(returnCode))")
                continue
            }
            
            print(Utils.now(), "Device Removed: ", name)
            
            // Notify we've seen a device disconnect
            NotificationCenter.default.post(name: .USBDeviceDisconnected, object: [
                "id": deviceId
            ])
        }
    }
    
    @objc func run() {
        // Create notification port for callbacks and add it as runloop source
        let notifyPort: IONotificationPortRef = IONotificationPortCreate(kIOMasterPortDefault)
        let runLoopSource = IONotificationPortGetRunLoopSource(notifyPort).takeUnretainedValue()
        CFRunLoopAddSource(CFRunLoopGetCurrent(), runLoopSource, CFRunLoopMode.defaultMode)
        
        IONotificationPortSetDispatchQueue(notifyPort, DispatchQueue(label: "IODetector"))
        print(Utils.now(), "Notifying on port", notifyPort)
        
        // Build Match Spec
        let matchSpec = IOServiceMatching(kIOUSBDeviceClassName) as NSMutableDictionary
        matchSpec[kUSBVendorID] = NSNumber(value: vendorId)
        matchSpec[kUSBProductID] = NSNumber(value: productId)
        
        print(Utils.now(), "Matching devices:", matchSpec)
        
        // Create Context for Callbacks
        var addedIterator: io_iterator_t = 0
        var removedIterator: io_iterator_t = 0
        let ctx = unsafeBitCast(self, to: UnsafeMutableRawPointer.self)
        
        // Hookup Device FirstMatch Callback
        let onDeviceAdded: IOServiceMatchingCallback = { ctx, iterator in
            unsafeBitCast(ctx, to: USBDeviceDaemon.self).onDeviceAdded(iterator)
        }
        IOServiceAddMatchingNotification(
            notifyPort,
            kIOFirstMatchNotification,
            matchSpec,
            onDeviceAdded,
            ctx,
            &addedIterator
        )
        
        // Hookup Device Termination Callback
        let onDeviceRemoved: IOServiceMatchingCallback = { ctx, iterator in
            unsafeBitCast(ctx, to: USBDeviceDaemon.self).onDeviceRemoved(iterator)
        }
        IOServiceAddMatchingNotification(
            notifyPort,
            kIOTerminatedNotification,
            matchSpec,
            onDeviceRemoved,
            ctx,
            &removedIterator
        )
        
        // Find Currently Plugged-In Devices
        self.onDeviceAdded(addedIterator)
        self.onDeviceRemoved(removedIterator)
        
        // Run!
        RunLoop.current.run()
    }
    
    func start() {
        let daemon = Thread(
            target: self,
            selector: #selector(self.run),
            object: nil
        )
        
        daemon.start()
        
        print(Utils.now(), "Started USB Device Daemon...")
    }
}
