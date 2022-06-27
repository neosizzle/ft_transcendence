import { Injectable, UnauthorizedException } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';
import { PrismaService } from 'src/prisma/prisma.service';

interface TokenInfo {
	access_token? : string,
	token_type? : string,
	expires_in? : number,
	refresh_token? : string,
	scope? : string,
	created_at? : number,
	error? : string,
	error_description? : string,
}


@Injectable()
export class AuthService {
	constructor(private prisma : PrismaService, private config : ConfigService){}

	// TODO : TEST THIS
	private async getTokenInfo(code : string) : Promise<TokenInfo> {
		const payload = new FormData();

		payload.append('grant_type', 'authorization_code');
		payload.append('client_id', this.config.get("API_UID_42"));
		payload.append('client_secret', this.config.get("API_SECRET_42"));
		payload.append('code', code);
		payload.append('redirect_uri', this.config.get('API_REDIR_URI_42'));

		return fetch(`${this.config.get('API_HOST_42')}/oauth/token`, {
			method : "POST",
			body : payload
		}).then(res => res.json())
	}

	async authenticate(code : string)
	{
		let	tokenInfo : TokenInfo;
		let	userInfo : any;

		// fetch token with code
		try {
			tokenInfo = await this.getTokenInfo(code);
			if (tokenInfo.error) throw new Error(JSON.stringify(tokenInfo))
		} catch (error) {
			// send error if code is invalid

			console.error(error.message);
			throw new UnauthorizedException(error.message);
		}

		// get 42 user info 
		try {
			const res = await fetch(`${this.config.get('API_HOST_42')}/oauth/token`, {
				method : "GET",
				headers: {
					'Authorization': `Bearer ${tokenInfo.access_token}`
					},
			})

			const userInfo = await res.json();
			if (userInfo.error) throw new Error(JSON.stringify(userInfo))
		} catch (error) {
			// send error if request fails
			console.error(error.message);
			throw new UnauthorizedException(error.message);
		}

		// get or create user in db
		

		// calculate expire date token

		// add or update auth entry

		// return access token and expire date
		console.log(tokenInfo)
		return {data : tokenInfo.access_token}
	}
}
