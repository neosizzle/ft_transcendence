import { Injectable } from "@nestjs/common";
import * as moment from "moment";

@Injectable()
export class AppService {
  getHello(): string {
    const res = `Hello ${moment().format()}`;
    return res;
  }
}
