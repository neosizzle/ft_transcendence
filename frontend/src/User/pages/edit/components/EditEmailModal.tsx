import React, { FunctionComponent, useState } from "react";
import { API_ROOT } from "../../../../constants";
import validator from 'validator'
import { auth_net_post } from "../../../../utils";
import { useNavigate } from "react-router-dom";

interface EditEmailModalProps {
	setOpenEditEmailModal : React.Dispatch<React.SetStateAction<boolean>>;
	setNewEmail : React.Dispatch<React.SetStateAction<string | null>>;
}

const otpEndpoint = `${API_ROOT}/otp`
const OTP_LENGTH = 4

const EditEmailModal: FunctionComponent<EditEmailModalProps> = ({setOpenEditEmailModal, setNewEmail}) => {
	const [loading, setLoading] = useState<boolean>(false);
	const [currSlide, setCurrSlide] = useState<number>(0);
	const [emailErr, setEmailErr] = useState<string | null>(null); // email error state
	const [currEmail, setCurrEmail] = useState<string>(""); // current email to verify
	const [otpErr, setOtpErr] = useState<string | null>(null); // otp error state
	const [otpSuccess, setOtpSuccess] = useState<boolean>(false); // otp sucess status
	const [currOtp, setCurrOtp] = useState<string>(""); // otp email to verify
	const [otpKey, setOtpKey] = useState<string | null>(null); // otp key
	const navigate = useNavigate();

	const sendOtp = () => {
		auth_net_post(`${otpEndpoint}/request`, {email : currEmail})
		.then((data)=>
		{
			if (data.error && data.error === "Forbidden") navigate("/logout");
			setOtpKey(data.data.key);
		})
	}

	const verifyOtp = async (otp : string) => {
		setLoading(true)
		const res = await auth_net_post(`${otpEndpoint}/verify`, {check : currEmail, key : otpKey, otp})
		
		if (res.error && res.error === "Forbidden") navigate("/logout");
		else if (res.error && res.error === "Bad Request") setOtpErr("OTP invalid");
		else if (res.data){
			setOtpSuccess(true);
			setOtpErr(null);
		}
		else setOtpErr(res.error ? res.error : "Something wrong happened");
		setLoading(false)
		
	}
	
	return ( 
	<div className="fixed top-0 left-0 right-0 z-50 h-full overflow-x-hidden overflow-y-auto md:inset-0  bg-gray-300/50">
		<div className="relative w-full p-4 h-full flex justify-center item-center">

			{/* Card */}
			<div className="relative bg-white rounded-lg h-3/4 shadow dark:bg-gray-700">

				{/* X Button */}
				<button onClick = {()=>setOpenEditEmailModal(false)} type="button" className="absolute top-3 right-2.5 text-gray-400 bg-transparent hover:bg-gray-200 hover:text-gray-900 rounded-lg text-sm p-1.5 ml-auto inline-flex items-center dark:hover:bg-gray-800 dark:hover:text-white" data-modal-toggle="popup-modal">
					<svg className="w-5 h-5" fill="currentColor" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg"><path fillRule="evenodd" d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z" clipRule="evenodd"></path></svg>  
				</button>

				{/* Content */}
				<div className="p-6 h-full  grid grid-rows-6 text-center gap-4">
					
					{
						!currSlide ?

						// Email slide
						<div className="row-span-5 sm:w-96 pt-10">
							<span className="text-lg font-medium">Please enter your new email</span>
							<div>
								<input className={`bg-white appearance-none border-2 mt-2 ${emailErr ? "border-red-200" : "border-gray-200"} rounded  py-2 px-4 text-gray-700 leading-tight`}
								type="text"
								value={currEmail || ""}
								onChange = {(e)=>setCurrEmail(e.target.value)}
								onKeyDown={(e)=>{
									if (e.key === 'Enter') {
										if (!validator.isEmail(currEmail))
											setEmailErr("Not a valid email")
										else
										{
											setEmailErr(null)
											setCurrSlide(1)
											sendOtp()
										}
									}
								}}
								/>
								{emailErr ? <span className="block text-sm text-red-700">{emailErr}</span> : null}
							</div>
						</div> :

						// OTP slide
						<div className="row-span-5 sm:w-96 p-5 pt-10">
							<span className="text-lg font-medium">Please enter the OTP sent to your email</span>
							<input maxLength={OTP_LENGTH} disabled={otpSuccess || loading} className={`bg-white appearance-none border-2 mt-2 ${otpErr ? "border-red-200" : otpSuccess ? "border-green-200" : "border-gray-200"} rounded  py-2 px-4 text-gray-700 leading-tight`}
								type="text"
								value={currOtp || ""}
								onChange = {(e)=>{
									setCurrOtp(e.target.value);

									if (e.target.value.length == OTP_LENGTH)
										verifyOtp(e.target.value);
								}}
								/>
							{otpErr ? <span className="block text-sm text-red-700">{otpErr}</span> : null}
							{otpSuccess ? <span className="block text-sm text-green-700">OTP validated</span> : null}
						</div>
					}

					<div className="row-span-1 flex justify-between">
					<button type="button"
					onClick={()=>{
						if (otpSuccess) return;
						if (currSlide) setCurrSlide(0)
					}} className={`${loading ? "opacity-50" : ""} font-medium rounded-lg text-sm inline-flex items-center px-5 py-2.5 text-center mr-2`}>
						{currSlide ? "Previous" : "Cancel"}
					</button>
					<button type="button"
					onClick={()=>{
						if (!currSlide)
						{
							if (!validator.isEmail(currEmail))
								setEmailErr("Not a valid email")
							else
							{
								setEmailErr(null)
								setCurrSlide(1)
								sendOtp()
							}
						}
						else
						{
							if (!otpSuccess) return ;
							else
							{
								setNewEmail(currEmail);
								setOpenEditEmailModal(false);
							}
						}
					}}
					className={`${loading ? "opacity-50" : ""} font-medium rounded-lg text-sm inline-flex items-center px-5 py-2.5 text-center mr-2`}>
						{currSlide ? "OK" : "Next"}
					</button>					
					</div>
				</div>
			</div>
		</div>
	</div>
	);
}
 
export default EditEmailModal;