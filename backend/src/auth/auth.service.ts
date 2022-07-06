import { Injectable, UnauthorizedException } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';
import { User, UserStatus } from '@prisma/client';
import * as moment from "moment";

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

interface UserInfo {
	intraId? : string,
	intraName? : string,
	error? : string,
	error_description? : string,
}

@Injectable()
export class AuthService {
	constructor(private prisma : PrismaService, private config : ConfigService){}

	/**
	 * gets access token via 42 oauth api
	 * 
	 * @param code code generataed by 42 oauth ui
	 * @returns access token used to access 42 api content
	 */
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

	/**
	 * Gets 42 user infromation using 42 api
	 * 
	 * @param token token obtained through 42 oauth api
	 * @returns 42 user 
	 */
	private async getUserInfo(token : string) : Promise<UserInfo> {
		return fetch(`${this.config.get('API_HOST_42')}/v2/me`, {
			method : "GET",
			headers: {
				'Authorization': `Bearer ${token}`
				},
		})
		.then(res => res.json())
		.then(data => {
			const res : UserInfo = {
				intraId : data.id,
				intraName : data.login,
				error : data.error,
				error_description : data.error_description,
			}

			return res;
		})
		.catch(e => {
			console.error(e)
			const res : UserInfo = {
				error : "fetch error",
				error_description : e.message,
			}
			return res;
		})
	}

	/**
	 * Obtains token using the code given and obtain 42 user.
	 * Check if user exists in our db and creates that use in our database if it doesnt.
	 * Add new entry in auth table relating the user and the token
	 * 
	 * @param code the code that was obtained via 42 Oauth UI
	 * @returns token as well as token expiry date
	 */
	async authenticate(code : string)
	{
		let	tokenInfo : TokenInfo;
		let	userInfo : UserInfo;
		let	user	: User;

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
			userInfo = await this.getUserInfo(tokenInfo.access_token);
			if (userInfo.error) throw new Error(JSON.stringify(userInfo))
		} catch (error) {
			// send error if request fails
			console.error(error.message);
			throw new UnauthorizedException(error.message);
		}

		// get or create user in db
		if (await this.prisma.user.count({where : {intraID : userInfo.intraId.toString()}}) === 0)
		{
			user = await this.prisma.user.create({
				data: {
					intraID : userInfo.intraId.toString(),
					intraName : userInfo.intraName,
					username : userInfo.intraName
				}
			});
		}
		else
			user = await this.prisma.user.findFirst({where : {intraID : userInfo.intraId.toString()}});

		//set user status in bg
		await this.prisma.user.update({where : {id : user.id}, data : { status : UserStatus.LOGGEDIN }});

		// calculate expire date token
		const expiresAt = moment().add(tokenInfo.expires_in, 's')

		// add or update auth entry
		const auth = await this.prisma.auth.upsert({
			where : {userId: user.id},
			create : {expiresAt: expiresAt.toISOString(), token: tokenInfo.access_token, userId: user.id},
			update: {expiresAt: expiresAt.toISOString(), token: tokenInfo.access_token}
		})

		// return access token and expire date
		return {data : {token : auth.token, expiresAt : auth.expiresAt}}
	}

	/**
	 * Logs out user by deleting the auth record 
	 * Set user status to logout
	 * 
	 * @param user User object from req
	 * @returns the expired token
	 */
	async logout(user : User) {
		const res = await this.prisma.auth.delete({where : {userId : user.id}});
		await this.prisma.user.update({where : {id : user.id}, data : {status : UserStatus.OFFLINE}})
		return {data : {token : res.token}}
	}
}
