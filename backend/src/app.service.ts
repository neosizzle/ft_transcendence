import { Injectable } from '@nestjs/common';
import * as moment from "moment";

@Injectable()
export class AppService {
  getHello(): string {
    let res : string;

    res = `Hello ${moment().format()}`;
    return res
  }
}
