import { Injectable, Logger } from "@nestjs/common";
import { ConfigService } from "@nestjs/config";
import * as Nodemailer from "nodemailer";

@Injectable()
export class MailService {
  constructor(private config: ConfigService) {}

  from = this.config.get("NODEMAILER_EMAIL");
  password = this.config.get("NODEMAILER_PASS");
  logger: Logger = new Logger(MailService.name);
  
  async sendEmail(to: string, content: string, subject: string) {
    // create reusable transporter object using the default SMTP transport
    const transporter = Nodemailer.createTransport({
      service: "gmail",
      auth: {
        user: this.from,
        pass: this.password,
      },
    });

    // send mail with defined transport object
    const info = await transporter.sendMail({
      from: this.from, // sender address
      to: to, // list of receivers
      subject: subject, // Subject line
      text: content, // plain text body
    });
    this.logger.log("Message sent: "+ info.messageId)
  }
}
