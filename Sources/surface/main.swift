import IOKit
import IOKit.usb
import IOKit.usb.IOUSBLib
import USBDeviceSwift
import Cocoa

enum EXIIReport: UInt8 {
    case CoordinateData=1   // Get Raw and Adjusted Coordinates from the device
    case Calibrate=4        // Calibrate device
    case ControllerStatus=6 // Get the controller status
    case Reset=7            // Reset the device!
    case RestoreDefaults=8
    case ControllerID=10
    case ReadParameter=16
}

enum EXIIReqType: UInt8 {
    case ToDevice = 0b01000000
    case WithResponse = 0b11000000
}

enum EXIICalibrationMode: UInt16 {
    case Extended=1
    case Corner=2
}

enum EXIIResetMode: UInt16 {
    case SoftReset=1
    case HardReset=2
}

enum EXIIDeviceError: Error {
    case DeviceInterfaceNotFound
    case RequestError(desc: String)
}

enum EXIIPowerOnCheckStatus : UInt8 {
    case RAMDataError = 0
    case RAMCodeError = 1
    case StrayError = 2
    case InvalidParametersError = 3
    case HardwareError = 4
    case Reserved_1 = 5
    case CableNOVRAMError = 6
    case InvalidLinearizationDataError = 7
    case NotUsed = 8
    case EEPROMNotWorking = 9
    case InvalidControllerID = 10
    case InvalidInterfaceConfigurationValues = 11
    case InvalidTouchConfigurationValues = 12
    case Reserved_2 = 13
    case ResistanceError = 14
    case BackplaneError = 15
    case CornerError = 16
    case FixedCalibrarionError = 17
}

enum EXIICommandStatus: UInt8 {
    case Failure = 0
    case CommandBeingProcessed = 1
    case Stage1ProcessingComplete = 2
    case CommandComplete = 3
    case SoftResetOccurred = 4
    case HardResetOccurred = 5
}

struct EXIIDeviceStatus {
    public var reportId : EXIIReport
    public var powerOnCheckStatus : EXIIPowerOnCheckStatus
    public var lastCommandStatus : EXIICommandStatus
    public var touchStatus : UInt8
    public var asyncReports : UInt16
}

class EXIIDevice {
    var deviceInfo: USBDevice
    
    required init(_ deviceInfo:USBDevice) {
        self.deviceInfo = deviceInfo
    }
    
    func reqSync(request: UnsafeMutablePointer<IOUSBDevRequest>?) throws -> Void {
        guard let deviceInterface = self.deviceInfo.deviceInterfacePtrPtr?.pointee?.pointee else {
            throw EXIIDeviceError.DeviceInterfaceNotFound
        }
        
        let kr:Int32 = deviceInterface.DeviceRequest(self.deviceInfo.deviceInterfacePtrPtr, request)
        
        if (kr != kIOReturnSuccess) {
            throw EXIIDeviceError.RequestError(desc: String(kr))
        }
    }
    
    func reset() throws -> Void {
        var request = IOUSBDevRequest(bmRequestType: EXIIReqType.ToDevice.rawValue,
                                      bRequest: EXIIReport.Reset.rawValue,
                                      wValue: EXIIResetMode.SoftReset.rawValue,
                                      wIndex: 0,
                                      wLength: 0,
                                      pData: nil,
                                      wLenDone: 0)
        
        try reqSync(request: &request)
    }
    
    func status() throws -> EXIIDeviceStatus {
        let length: Int = 8
        var data: [UInt8] = [UInt8](repeating: 0, count: length)
        var request = IOUSBDevRequest(bmRequestType: EXIIReqType.WithResponse.rawValue,
                                      bRequest: EXIIReport.ControllerStatus.rawValue,
                                      wValue: 0,
                                      wIndex: 0,
                                      wLength: UInt16(length),
                                      pData: &data,
                                      wLenDone: UInt32(length))
        
        try reqSync(request: &request)
        
        return EXIIDeviceStatus.init(
            reportId: EXIIReport.init(rawValue: data[0])!,
            powerOnCheckStatus: EXIIPowerOnCheckStatus.init(rawValue: data[2] << 2 & data[1])!,
            lastCommandStatus: EXIICommandStatus.init(rawValue: data[3])!,
            touchStatus: data[4],
            asyncReports: UInt16(data[6] << 2 & data[5])
        )
    }
}

let vendorId = 0x0596
let productId = 0x0001

class DeviceDaemon {
    let exiiDeviceMon = USBDeviceMonitor([
        USBMonitorData(vendorId: 0x0596, productId: 0x0001),
    ])

    func start() {
        let deviceDaemon = Thread(
            target: exiiDeviceMon,
            selector:#selector(exiiDeviceMon.start),
            object: nil)

        deviceDaemon.start()
        
        print(Utils.now(), "Daemon started...", deviceDaemon)
    }
}

class Utils {
    static func now() -> String {
        return String(Date().timeIntervalSince1970 * 100000)
    }
}

class CLI {
    var devices: [EXIIDevice] = []

    init() {
        NotificationCenter.default.addObserver(
            self,
            selector: #selector(self.addDevice),
            name: .USBDeviceConnected,
            object: nil)
    }
    
    @objc func addDevice(notification: NSNotification) {
        guard let nobj = notification.object as? NSDictionary else {
            return
        }
        guard let deviceInfo:USBDevice = nobj["device"] as? USBDevice else {
            return
        }
        print(Utils.now(), "Registering device ", deviceInfo)
        let device = EXIIDevice(deviceInfo)
        do {
            print(Utils.now(), "Resetting device...")
            try device.reset()
        } catch {
            print(Utils.now(), error)
        }
        self.devices.append(device)
    }
    
    func poll() {
        while true {
            devices.forEach { (device) in
                do {
                    let res = try device.status()
                    print(Utils.now(), res)
                } catch {
                    print(Utils.now(), "Device might still be resetting!", error)
                }
            }
            usleep(1000000)
        }
        
    }
}

DeviceDaemon.init().start()

CLI.init().poll()
