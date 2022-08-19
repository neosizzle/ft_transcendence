import { BadRequestException, Injectable } from "@nestjs/common";
import { PrismaService } from "src/prisma/prisma.service";
import { OtpRequestDto, OtpVerifyDto } from "./dto/otp.dto";
import { OTP } from "@prisma/client";
import { ConfigService } from "@nestjs/config";
import * as CryptoJS from "crypto-js";
import * as otpGenerator from "otp-generator";
import { MailService } from "src/mail/mail.service";

interface OTPKey {
  id: number;
  check: string;
  expTime: Date;
}
// The expiration time in minutes
const OTP_EXPIRATION_TIME = 10;

// milliseconds to minutes conversion
const MS_TO_MINS = 60000;

// To add minutes to the current time
const AddMinutesToDate = (date: Date, minutes: number): Date =>
  new Date(date.getTime() + minutes * MS_TO_MINS);

// encrypts a raw json object
const genEncKey = (
  id: number,
  model: OTP,
  check: string,
  secret: string
): string => {
  const res: OTPKey = {
    id,
    check,
    expTime: model.exprationTime,
  };

  return CryptoJS.AES.encrypt(JSON.stringify(res), secret).toString();
};

// decrypts a raw json object
const decodeEncKey = (key: string, secret: string) => {
  const decrypted = CryptoJS.AES.decrypt(key, secret);
  return JSON.parse(decrypted.toString(CryptoJS.enc.Utf8));
};

@Injectable()
export class OtpService {
  constructor(
    private prisma: PrismaService,
    private config: ConfigService,
    private mailService: MailService
  ) {}

  // request otp
  async requestOtp(dto: OtpRequestDto) {
    // gen exp date
    const now: Date = new Date();
    const expDate: Date = AddMinutesToDate(now, OTP_EXPIRATION_TIME);

    // generate OTP
    const otp = otpGenerator.generate(4, {
      digits: true,
      lowerCaseAlphabets: false,
      specialChars: false,
      upperCaseAlphabets: false,
    });

    // save generated otp in db
    const res = await this.prisma.oTP.create({
      data: {
        otp: CryptoJS.SHA256(otp).toString(),
        exprationTime: expDate,
      },
    });

    // send otp via email
    await this.mailService.sendEmail(
      dto.email,
      `Your otp is ${otp} `,
      "42Pong OTP"
    );
    return {
      data: {
        key: genEncKey(res.id, res, dto.email, this.config.get("AES_KEY_OTP")),
        check: dto.email,
      },
    };
  }

  // verify otp
  async verifyOtp(dto: OtpVerifyDto) {
    let decodedKey: OTPKey;

    //check if check is correct
    try {
      decodedKey = decodeEncKey(dto.key, this.config.get("AES_KEY_OTP"));
      if (decodedKey.check != dto.check)
        throw new BadRequestException("Bad Check");
    } catch (error) {
      if (error.message === "Malformed UTF-8 data")
        throw new BadRequestException("Bad Key");
    }

    // verify otp
    if (!decodedKey) throw new BadRequestException("Bad Key");
    const OTPRes = await this.prisma.oTP.findUnique({
      where: {
        id: decodedKey.id,
      },
    });
    if (!OTPRes) throw new BadRequestException("OTP not found");

    // check otp correctness
    if (CryptoJS.SHA256(dto.otp).toString() != OTPRes.otp)
      throw new BadRequestException("OTP Invalid");

    // check for otp verified
    if (OTPRes.verified) throw new BadRequestException("OTP Used");

    // check for OTP expiry date
    const now = new Date();
    if (OTPRes.exprationTime.getTime() < now.getTime())
      throw new BadRequestException("OTP Expired");

    // update verified status
    const res = await this.prisma.oTP.update({
      where: {
        id: OTPRes.id,
      },
      data: {
        verified: true,
      },
    });

    return { data: res };
  }
}
