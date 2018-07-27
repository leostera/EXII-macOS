//
import Cocoa
//  EXII.swift
//  surface
//
//  Created by Leandro Ostera on 2018-07-24.
//
import Foundation
import IOKit
import IOKit.usb
import IOKit.usb.IOUSBLib

enum EXIIReport: UInt8 {
    case CoordinateData = 1 // Get Raw and Adjusted Coordinates from the device
    case Calibrate = 4 // Calibrate device
    case ControllerStatus = 6 // Get the controller status
    case Reset = 7 // Reset the device!
    case RestoreDefaults = 8
    case ControllerID = 10
    case ReadParameter = 16
}

enum EXIIReqType: UInt8 {
    case HostToDevice = 0b01000000
    case DeviceToHost = 0b11000000
}

enum EXIICalibrationMode: UInt16 {
    case Extended = 1
    case Corner = 2
}

enum EXIIResetMode: UInt16 {
    case SoftReset = 1
    case HardReset = 2
}

enum EXIIError: Error {
    case DeviceInterfaceNotFound
    case InterfaceInterfaceNotFound
    case InterfaceInterfaceError(code: Int32)
    case RequestError(desc: String)
}

enum EXIIPowerOnCheckStatus: UInt8 {
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
    public var reportId: EXIIReport
    public var powerOnCheckStatus: EXIIPowerOnCheckStatus
    public var lastCommandStatus: EXIICommandStatus
    public var touchStatus: UInt8
    public var asyncReports: UInt16
}

enum EXIITouchStatus: UInt8 {
    case TouchDown = 0
    case LiftUp = 1
}

struct EXIICoordinateData {
    public var reportId: EXIIReport
    public var loopCounter: UInt8
    public var touchStatus: EXIITouchStatus
    public var xCompensated: UInt16
    public var yCompensated: UInt16
    public var xRaw: Int16
    public var yRaw: Int16
}

class EXIIDevice {
    var deviceInfo: USBDevice

    required init(_ deviceInfo: USBDevice) {
        self.deviceInfo = deviceInfo
    }

    func readPipe(pipeRef: UInt8, data: UnsafeMutableRawPointer!, size: UnsafeMutablePointer<UInt32>?) throws {
        guard let interfaceInterface = self.deviceInfo.interfaceInterfacePtrPtr?.pointee?.pointee else {
            throw EXIIError.InterfaceInterfaceNotFound
        }

        let kr: Int32 = interfaceInterface.ReadPipe(
            deviceInfo.interfaceInterfacePtrPtr,
            pipeRef,
            data,
            size
        )

        if kr != kIOReturnSuccess {
            print(kr)
            throw EXIIError.InterfaceInterfaceError(code: kr)
        }
    }

    func reqSync(request: UnsafeMutablePointer<IOUSBDevRequest>?) throws {
        guard let deviceInterface = self.deviceInfo.deviceInterfacePtrPtr?.pointee?.pointee else {
            throw EXIIError.DeviceInterfaceNotFound
        }

        let kr: Int32 = deviceInterface.DeviceRequest(deviceInfo.deviceInterfacePtrPtr, request)

        if kr != kIOReturnSuccess {
            throw EXIIError.RequestError(desc: String(kr))
        }
    }

    func reset() throws {
        var request = IOUSBDevRequest(
            bmRequestType: EXIIReqType.HostToDevice.rawValue,
            bRequest: EXIIReport.Reset.rawValue,
            wValue: EXIIResetMode.SoftReset.rawValue,
            wIndex: 0,
            wLength: 0,
            pData: nil,
            wLenDone: 0
        )

        try reqSync(request: &request)
    }

    func status() throws -> EXIIDeviceStatus {
        let length: Int = 8
        var data: [UInt8] = [UInt8](repeating: 0, count: length)
        var request = IOUSBDevRequest(
            bmRequestType: EXIIReqType.DeviceToHost.rawValue,
            bRequest: EXIIReport.ControllerStatus.rawValue,
            wValue: 0,
            wIndex: 0,
            wLength: UInt16(length),
            pData: &data,
            wLenDone: UInt32(length)
        )

        try reqSync(request: &request)

        return EXIIDeviceStatus(
            reportId: EXIIReport(rawValue: data[0])!,
            powerOnCheckStatus: EXIIPowerOnCheckStatus(rawValue: data[2] << 2 & data[1])!,
            lastCommandStatus: EXIICommandStatus(rawValue: data[3])!,
            touchStatus: data[4],
            asyncReports: UInt16(data[6] << 2 & data[5])
        )
    }

    func coordinate() throws -> EXIICoordinateData? {
        var length: UInt32 = 32
        var data: [UInt8] = [UInt8](repeating: 0, count: Int(length))
        for p in deviceInfo.pipes {
            do {
                try readPipe(pipeRef: p.pipeRef, data: &data, size: &length)
                let touchStatus = (data[2] << 6) == 0b01000000 ? EXIITouchStatus.LiftUp : EXIITouchStatus.TouchDown
                return EXIICoordinateData(
                    reportId: EXIIReport.CoordinateData,
                    loopCounter: data[1],
                    touchStatus: touchStatus,
                    // All operations below are Least Significant Byte first
                    xCompensated: UInt16(data[4]) << 8 + UInt16(data[3]),
                    yCompensated: UInt16(data[6]) << 8 + UInt16(data[5]),
                    xRaw: Int16(data[8] ) << 8 + Int16(data[7]),
                    yRaw: Int16(data[10]) << 8 + Int16(data[9])
                )
            } catch {
                print(Utils.now(), "Error reading from pipe", p.pipeRef)
            }
        }
        return Optional.none
    }
}
