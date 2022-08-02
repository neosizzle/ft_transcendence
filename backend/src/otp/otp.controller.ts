import { Body, Controller, Post, UseGuards } from '@nestjs/common';
import { AuthGuard } from 'src/users/auth/guard';
import { OtpRequestDto, OtpVerifyDto } from './dto/otp.dto';
import { OtpService } from './otp.service';

@Controller('/api/v1/otp')
@UseGuards(AuthGuard)
export class OtpController {
	constructor(private otpService : OtpService){}

	// request otp
	@Post('request')
	requestOtp(@Body() body : OtpRequestDto)
	{
		return this.otpService.requestOtp(body);
	}

	// verify otp
	@Post('verify')
	verifyOtp(@Body() body : OtpVerifyDto)
	{
		return this.otpService.verifyOtp(body);
	}
}
